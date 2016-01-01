# fsrest - A Filesystem-focused RESTful Web Server

fsrest is a webserver that:

 - encourages RESTful website/application design
 - leverages the structure of the filesystem
 - is not tied to any programming language

You can use fsrest as a lightweight static web server (with a twist) or to build dynamic sites or modular web services.

## How it Works

fsrest starts with a simple concept: directories are resources and files are representations. For example, if you want to serve a page at `/home`, instead of creating a `home.html` or `home.php` file, you'd create a `home` directory at the top of your web root and then place a representation of the home page within the directory.

```
$ mkdir home
$ cat > home/text.html
<!DOCTYPE html><html><head><title>Home Page</title></head><body></body></html>
^D
```

The `home` directory is the resource represented by the URL `/home`. It has an HTML representation in the file `text.html`. `text.html` wasn't chosen at random: it's the MIME type for HTML (but with the `/` replaced by a `.`).

Let's take a look at a simplified HTTP request:

```
GET /home HTTP/1.1
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Encoding: gzip, deflate
Accept-Language: en-us,en;q=0.5
```

The client is stating that it would prefer to receive HTML, XHTML, or XML, but that anything (`*/*`) will do if those aren't available. When fsrest receives this request, it checks the `home` directory to see what's available. It determines that `text/html` is the best match for the client's request and sends it back to the client.

Let's test this now by starting fsrest in the current directory listening on port 80 on all network interfaces. fsrest expects the top-level directory to have one directory for each hostname that it's serving, so we first need to create a `localhost` directory (since that's where we'll be doing our testing). Then we'll change into that directory for the rest of the examples.

```
$ mkdir localhost
$ mv home localhost/
$ sudo fsrest . 0.0.0.0 80 &
[1] 39112
$ cd localhost
```

And now we'll use curl to make a request:

```
$ curl -v http://localhost/home
* Connected to localhost (127.0.0.1) port 80 (#0)
> GET / HTTP/1.1
> Host: localhost
> User-Agent: curl/7.43.0
> Accept: */*
>
< HTTP/1.1 200 OK
< Content-Type: text/html; charset=utf-8
< Transfer-Encoding: Chunked
<
<!DOCTYPE html><html><head><title>Home Page</title></head><body></body></html>
* Connection #0 to host localhost left intact
```

Note that fsrest assumes the UTF-8 character set for all representations.

### Multiple Representations

Here's another hypothetical request:

```
GET /image/logo.gif HTTP/1.1
Accept: image/png,image/*;q=0.8,*/*;q=0.5
Accept-Encoding: gzip, deflate
Accept-Language: en-us,en;q=0.5
```

This HTTP client is requesting an image. It knows it's requesting an image (probably because it found the URL in an `<img>` `src` attribute), and it passes along details about the type of images it prefers and will accept. But the URL explicitly mentions logo.gif, and most likely the web server on the other end is going to ignore any `Accept` headers (since it can't do anything with it anyway: there's only 1 way to serve a GIF off the filesystem).

With fsrest, we instead write the following HTML, leaving off a file extension:

```
<img src="/image/logo" alt="logo" />
```

Then, when the client requests the file, we can find a representation that best matches what the client wants. Maybe the client prefers PNG to GIF. To do this, first  we'd create a directory named `image/logo` at the top of our web root.

```
$ mkdir -p image/logo
$ mv logo.gif image/logo/image.gif
```

Now, when a client requests the resource at `/image/logo`, fsrest will serve the GIF with `Content-Type: image/gif`. To provide a PNG as well, just put an alternate representation of the image in the directory:

```
$ mv logo.png image/logo/image.png
```

At this point, when the client requests `/logo/image`, fsrest has 2 representations to choose from, and it will pick which to use based on the client's request. For example, if the client sends:

```
GET /image/logo HTTP/1.1
Accept: image/png,image/*;q=0.8,*/*;q=0.5
Accept-Encoding: gzip, deflate
Accept-Language: en-us,en;q=0.5
```

fsrest will determine, based on the `Accept` header, that `image/png` is the format most preferred by the client, and will send that representation along.

Remember that the filenames for each representation are actually the file's MIME type with the slash replaced by a dot. In each case so far it looks like we're just using file extensions, but fsrest is not looking at them that way. To really see this, let's add a 3rd representation of the image: a vector graphic.

```
$ mv logo.svg image/logo/image.svg+xml
```

And here you can see a clearer example of how fsrest looks at file names. The MIME type for SVG files is `image/svg+xml`. fsrest doesn't have a registry of MIME types; it simply looks at file names. You could create `image/logo/foo.bar` and if it were served back to the client, it would be sent as type `foo/bar`.

## Dynamic Resources

If the file corresponding to a resource is marked as executable, fsrest will execute it and return its output to the client. For example, let's create a search page that returns a list of all resources containing a given string.

```
$ mkdir search
$ cat > search/text.html
#!/usr/bin/env bash -e
echo "<h1>Search Results</h1><ul>"
for dir in $(find .. -type f ! -perm -a+x -exec fgrep -l "$QUERY_PARAM_Q" {} \; | \
               while read line; do
                 echo $(dirname $(echo $line | sed 's/^\.\.//'))
               done | uniq); do
  echo "<li><a href=\"$dir\">$dir</a></li>"
done
echo "</ul>"
^D
$ chmod +x text.html
```

To use the search page:

```
$ curl http://localhost/search?q=Page
<h1>Search Results</h1><ul>
<li><a href="/home">/home</a></li>
</ul>
```

This uses the `find` program to look for non-executable files containing a given string and then returns an HTML list of links to the resources. We currently only have one resource (home) that matches the given search. Ignoring the specifics of the bash code, note that:

* Everything output to stdout is sent as the body of the HTTP response (each of the echo lines).
* The query parameter `q` was accessed through the environment variable `QUERY_PARAM_Q`.
* If you examine the headers, the Content-Type will be `text/html; charset=utf-8` because of the filename of the script.
* The file is executed in its directory (this is true whenever a file is executed by fsrest).

Note: stderr is inherited by the executable, so any errors or anything you write to stderr will go to fsrest's stderr. If you're logging the stderr of fsrest you will also log errors from any executables executed by fsrest.

Dynamic resources don't need to be shell scripts; any executable will work. For example, you could compile a Haskell binary and place that in the directory instead. It would be able to access the `QUERY_PARAM_Q` environment variable via the Haskell `getEnv` function.

### Environment Variables

Let's look at some of the environment variables that are passed to the executable. The `env` program prints all environment variables, so we can just create a symlink to it for testing.

```
$ mkdir debug
$ ln -s /usr/bin/env debug/text.plain
$ curl http://localhost/debug?q=Page
RESOURCE=/debug
REPRESENTATION=text.plain
QUERY_STRING=q=Page
QUERY_JSON={"q":"Page"}
QUERY_PARAMS=Q
QUERY_PARAM_Q=Page
HEADERS=HOST USER_AGENT ACCEPT
HEADERS_JSON={"Accept":"*/*","User-Agent":"curl/7.43.0","Host":"127.0.0.1"}
HEADER_HOST=127.0.0.1
HEADER_USER_AGENT=curl/7.43.0
HEADER_ACCEPT=*/*
STATUS_FD=17
HEADERS_FD=19
```

* `RESOURCE`: the full HTTP path to the resource
* `REPRESENTATION`: the filename corresponding to the sent or requested representation
* `QUERY_STRING`: the raw query string or the empty string if no query was present
* `QUERY_JSON`: the query as a JSON object
* `QUERY_PARAMS`: a list of query parameter environment variable names separated by spaces (useful for iterating over all query parameters)
* `QUERY_PARAM_*`: the value of each query parameter (possibly empty)
* `HEADERS`: a list of headers in the request, separated by spaces (useful for iterating over all request headers)
* `HEADERS_JSON`: all headers as a JSON object
* `HEADER_*`: the value of each request header
* `STATUS_FD`: the file descriptor to write a response status code to (see next section)
* `HEADERS_FD`: the file descriptor to write response headers to (see next section)

Note that environment variables are strings. If you want to use the `*_JSON` variables as objects you will have to decode them first.

### Response Status Code and Headers

Since all output to stdout is sent as the response body, how do you override the HTTP response code and override/add response headers? fsrest sets the `STATUS_FD` and `HEADERS_FD` environment variables to the file descriptors of some pipes it has setup for this purpose. Here's an example of using them to initiate a redirect:

```
$ mkdir redirect
$ cat > redirect/text.plain
#!/usr/bin/env bash -e
echo "302" >&$STATUS_FD
echo "Location: /" >&$HEADERS_FD
^D
$ chmod +x redirect/text.plain
```

```
$ curl -v http://localhost/redirect
* Connected to localhost (127.0.0.1) port 80 (#0)
> GET /redirect HTTP/1.1
> Host: localhost
> User-Agent: curl/7.43.0
> Accept: */*
>
< HTTP/1.1 302 Found
< Content-Type: text/plain; charset=utf-8
< Location: http://localhost/
< Server: fsrest
< Transfer-Encoding: Chunked
<
* Connection #0 to host localhost left intact
```

We can use the `STATUS_FD` and `HEADERS_FD` variables in output redirection just like the standard file descriptors (1, 2, and 3). To make use of these when you're not using bash, you can use the corresponding `/dev/fd` devices.

```
$ mkdir debug-fd
$ cat > debug-fd/text.plain
#!/usr/bin/env bash -e
ls -l /dev/fd/$STATUS_FD /dev/fd/$HEADERS_FD
^D
$ chmod +x debug-fd/text.plain
```

```
$ curl http://localhost/debug-fd
prw-rw----  0 root  wheel  0 Dec 30 10:19 /dev/fd/17
prw-rw----  0 root  wheel  0 Dec 30 10:19 /dev/fd/19
```

Note: The files here are owned by root since we're running fsrest via sudo (so that we can listen to port 80). You should use a non-root user for anything other than testing.

Note: If your program exits with a non-0 code, the response code will be set to 500 (Internal Server Error) no matter what code you might have set.

## POSTing new resources

Each resource (directory) can have a `POST` executable (or symlink to an executable) that allows for POSTing of subordinate resources. The POST request body, if it exists, is passed to the executable's stdin.

Let's say you'd like to allow posting comments on the front page of your site. The directory that fsrest is serving might contain the single file `text.html`. This means that the root of the site (`/`) will be a simple HTML page. Let's have comments live under the subordinate `comments` resource.

```
$ mkdir comments
```

How are we going to post comments? Well, ignoring all the complexities of security and input validation, let's simply put the POSTed body into a new subordinate comment resource. Let's number each comment starting from 1.

```
$ cat > comments/POST
#!/usr/bin/env bash -e
largest=$(find . -iregex './[0-9]+' | sort -r | head -n 1)
if [ -z $largest ]; then
  new=1
else
  new=$(expr $(basename $largest) + 1)
fi
mkdir $new
cat > $new/$REPRESENTATION
echo "201" >&$STATUS_FD
echo "Location: /comments/$new" >&$HEADERS_FD
^D
$ chmod +x comments/POST
```

Make sure to set the `POST` script to executable. If it's not executable, fsrest will not use it and will respond to `POST` requests with `405 Method Not Allowed`.

```
$ curl -v -X POST -H 'Content-Type: text/plain' -d 'Hello, world.' http://localhost/comments
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 80 (#0)
> POST /comments HTTP/1.1
> Host: localhost
> User-Agent: curl/7.43.0
> Accept: */*
> Content-Type: text/plain
> Content-Length: 13
>
* upload completely sent off: 13 out of 13 bytes
< HTTP/1.1 201 Created
< Location: /comments/1
< Server: fsrest
< Transfer-Encoding: Chunked
<
* Connection #0 to host localhost left intact
```

If everything worked correctly, a new comment should appear in the filesystem as `comments/1/text.plain` and you should be able to `GET` it as well.

```
$ curl http://localhost/comments/1
Hello, world.
```

## PUTing new resources

PUT is a little different from POST because while we POST subordinate resources to an already-existing resource, PUT can either create a new resource or update an existing one. Because of this, it's more convenient to place the `PUT` script in the parent directory of the resource.

For example, let's implement a simple content management system where articles are created or edited with PUT operations. A client request might look like:

```
$ curl -X PUT -H 'Content-Type: text/html' -T rest-design.html http://localhost/articles/restful-api-design
```

This command will PUT the contents of the file `rest-design.html` (on the client's computer) to the given URL. We'll need the articles resource to exist and to place a `PUT` executable there.

```
$ mkdir articles
$ cat > articles/PUT
#!/usr/bin/env bash -e
mkdir -p $1
cat > $1/$REPRESENTATION
^D
$ chmod +x articles/PUT
```

The first argument to the executable (`$1`) is the bare name of the resource (`restful-api-design`). This is a convenience for PUT and DELETE executables that is equivalent to the basename of the `RESOURCE` environment varilable. It's useful since the handlers will usually need to create or delete the corresponding directory.

What about PUT for existing resources? You might want to allow editing of the comments from the earlier example with PUT. To replace the content that was created by the earlier POST, you could:

```
$ curl -X PUT -H 'Content-Type: text/plain' -d 'First post!' http://localhost/comments/1
```

Again, place a `PUT` script in the comment resource's parent directory:

```
$ cat > comments/PUT
#!/usr/bin/env bash -e
mkdir -p $1
cat > $1/$REPRESENTATION
^D
$ chmod +x comments/PUT
```

Note that the `PUT` script is identical in these simple cases but your `PUT` program might restrict the content type that each is allowed to have and do additional validation and transformation before storing the representation to the filesystem.

Note: It is up to you to make sure that your PUT operations are idempotent.

Note: A PUT in fsrest doesn't necessarily update a resource, it creates or updates a given representation.

## DELETEing resources

DELETE is implemented similar to POST. The `DELETE` script must also be in the parent directory of the resource to delete. For example, for deleting comments:

```
$ curl -X DELETE http://localhost/comments/1
```

Here's a simple (but unsafe) implementation of `DELETE`:

```
$ cat > comments/DELETE
#!/usr/bin/env bash -e
rm -rf $1
$ chmod +x DELETE
```

The first argument (`$1`) is the basename of the resource to delete (just as with PUT). Since DELETE requests deletion of an entire resource and not just a single representation, there is no representation environment variable.

## Getting Started

Build fsrest with `cabal install` or with `nix-build dev.nix`.

Start fsrest and provide it with the directory that you want to serve and give it an address and port number to listen on:

```
fsrest /var/www 0.0.0.0 80
```

### Installing on NixOS

`module.nix` provides a way for you to declaratively install and configure fsrest on a [NixOS](http://nixos.org/) system.

## Limitations

Here are some current limitations that might be removed in later versions.

 - Language negotiation based on `Accept-Language` is not implemented.
 - I recommend proxying requests to fsrest via a more mature web server for sanitization, logging and to enforce limits.

## Common Problems

### `fsrest: readProcess: some.file  (exit 127): failed`

This is probably happening because the file is marked as executable and fsrest is trying to execute it (and failing).

### Multiple Choices

If content negotiation (based on the request's `Accept` header) turns up multiple equally-good results, fsrest will respond with HTTP error code 300 ("Multiple Choices"). This is most likely to happen when a browser makes a request with `Accept: */*`. You can set a default representation for these situations by symlinking the preferred representation to a file named `GET`.
