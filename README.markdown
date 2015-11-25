# A RESTful Web Server Designed Around the Filesystem

fsrest is a webserver that:

 - encourages and assists with RESTful website/service design
 - provides pretty URLs for static files
 - enables dynamic sites without databases

fsrest is NOT:

 - complete (only GET and POST are currently implemented)
 - high performance
 - tied to any programming language

# Motivation and Philosophy

Most modern websites are served by monolithic programs. These programs are usually written in a single language and communicate with a database. Each new feature of the site increases the size and complexity of the program.

## The Right Tool for the Job

fsrest isn't tied to a particular programming language. It's job is just to dispatch HTTP requests to the appropriate place. Mostly, it serves static files. To support dynamic content, you can write POST handlers in any language that you can launch from the shell.

## RESTful by Design

Due to the way fsrest requires that resources (URLs) be directories, it's more difficult to create non-RESTful websites.

# How it Works

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

The client is stating that it would prefer to receive HTML, XHTML, or XML, but that anything (`*/*`) will do if those aren't available. When fsrest receives this request, it checks the `home` directory to see what's available. It determines that `text/html` is the best match for the client's request and sends it back to the client:

```
HTTP/1.1 200 OK
Content-Type: text/html; charset="UTF-8"
...
<!DOCTYPE html><html><head><title>Home Page</title></head><body></body></html>
```

(Note that fsrest assumes UTF-8 for all text/* representations.)

Here's another request:

```
GET /image/logo.gif HTTP/1.1
Accept: image/png,image/*;q=0.8,*/*;q=0.5
Accept-Encoding: gzip, deflate
Accept-Language: en-us,en;q=0.5
```

Now the HTTP client is requesting an image. It knows it's requesting an image (probably because it found the URL in an `<img>` `src` attribute), and it passes along details about the type of images it prefers and will accept. But the URL explicitly mentions logo.gif, and most likely the web server on the other end is going to ignore any `Accept` headers (since it can't do anything with it anyway: there's only 1 way to serve a GIF off the filesystem).

But wait, who needs pretty URLs for images? After all, how often are you going to see those in the address bar?

It's not about the URL being pretty, it's about it being flexible. You might remember the big patent scare surrounding GIFs in the 90s. Before PNGs gained popularity, most non-photographic images were served as GIFs. There was a panic that web masters were going to be sued by GIF patent holders, and there was a push to replace all GIFs with alternate image formats (such as JPEGs or PNGs). But this meant changing URLs as well. If you had an `<img src="/image/logo.gif">`, each place it was referenced would need to be replaced with `<img src="/image/logo.png">`. This might not have been a big deal, but it brings up the question: why not just serve the image at `/image/logo`? After all, the browser doesn't care about file extensions. However, the web server might. In fact, most web servers use the filename to determine the MIME type to send in the response to the client.

Say we want to serve a GIF image as `/image/logo`. With fsrest, we'd create a directory named `image/logo` at the top of our web root.

```
$ mkdir -p image/logo
$ mv ~/logo.gif image/logo/image.gif
```

Now, when a client requests the resource at `/image/logo`, fsrest will serve the GIF with `Content-Type: image/gif`. To provide a PNG as well, just put an alternate representation of the image in the directory:

```
$ mv ~/logo.png image/logo/image.png
```

Now, when the client requests `/logo/image`, fsrest has 2 representations to choose from, and it will pick which to use based on the client's request. For example, if the client sends:

```
GET /image/logo HTTP/1.1
Accept: image/png,image/*;q=0.8,*/*;q=0.5
Accept-Encoding: gzip, deflate
Accept-Language: en-us,en;q=0.5
```

fsrest will determine, based on the `Accept` header, that `image/png` is the format most preferred by the client, and will send that representation along.

Remember that the filenames for each representation are actually the file's MIME type with the slash replaced by a dot. In each case so far it looks like we're just using file extensions, but fsrest is not looking at them that way. To really see this, let's add a 3rd representation of the image: a vector graphic.

If you're familiar with vector graphics, you'll know that the advantage with them is that they can scale to a variety of sizes cleanly. With the increasing number of devices with high-resolution displays, they might soon become important. Let's add an SVG representation of the logo:

```
$ mv ~/logo.svg image/logo/image.svg+xml
```

And here you can see a clearer example of how fsrest looks at file names. The MIME type for SVG files is `image/svg+xml`. fsrest doesn't have a registry of MIME types; it simply looks at file names. You could create `image/logo/foo.bar` and if it were served back to the client, it would be sent as type `foo/bar`.

# POSTing new resources

Each resource (directory) can have a `POST` executable (or symlink to an executable) that allows for POSTing of subordinate resources. If it exists, any POST request is passed to the executable with the POST body, if any, passed into stdin. Any request variables are set as environment variables. (Note that file inputs are not currently supported).

Let's say you'd like to allow POSTing comments on the front page of your site. The directory that fsrest is serving might contain the single file `text.html`. This means that the root of the site (`/`) will be a simple HTML page. Let's have comments live under the subordinate `comments` resource.

```
$ mkdir comments
```

Right now there are no comments. But the comments resource should exist rather than give a 404. Even though we're not explicitly linking to `/comments`, let's try to keep resources meaningful.

```
$ echo "" > comments/text.plain
```

How are we going to post comments? Well, ignoring all the complexities of security and input validation, let's simply put the POSTed body into a new subordinate comment resource. Let's number each comment starting from 1.

```
$ cat > comments/POST
#!/bin/bash
largest=$(find . -iregex './[0-9]+' -printf "%f\n" | sort -r | head -n 1)
new=$(expr $largest + 1)
mkdir $new
cat > "$new/${CONTENT_TYPE/\//.})"
^D
$ chmod +x comments/POST
```

Let's break this down line by line:

 1. The first line is a shebang that lets the shell know that this is a bash script.
 2. Next, we use `find` to find the highest comment number already in the directory (the current working directory is the directory that the resource is being POSTed to).
 3. The next line sets `new` to 1 greater than `largest`. If largest was empty (because there were no existing comments) it sets it to `1`.
 4. Once we know the new comment number, we create it with `mkdir`.
 5. Finally, we store the body of the POST request into a file with the POSTed representation.
 6. We set the `POST` script to executable. If it's not executable, fsrest will not use it and will respond to `POST` requests with `405 Method Not Allowed`.

```
$ curl -H "Content-Type: text/plain" -X POST -d "Hello, world." http://yoursite/comments
1
```

If everything worked correctly, a new comment should appear in the filesystem as `comments/1/text.plain` and you should be able to `GET` it as well.

```
$ curl http://yoursites/comments/1
```

## POST variables

Any parameters POSTed to a resource will be available as variables.

```
$ echo > POST
#!/bin/bash
env
^D
$ chmod +x POST
$ curl -X POST -d "foo=bar&foos=ball" http://yoursite/
foos=ball
PWD=/home/jekor/project/jekor.lan/www
foo=bar
SHLVL=1
CONTENT_TYPE=application/x-www-form-urlencoded
...
```

`PWD`, `SHLVL`, and `_` are 3 environment variables that are set by Bash.

# Getting Started

Build fsrest with `cabal install`.

Point fsrest to the directory that you want to serve and give it an address and port number to listen on:

```
fsrest /var/www 0.0.0.0 80
```

# Limitations

Here are some current limitations that might be removed in later versions.

 - `PUT`, `DELETE`, etc. are not implemented.
 - Language negotiation based on `Accept-Language` is not implemented.

# Common Problems

## `fsrest: readProcess: some.file  (exit 127): failed`

This is probably happening because the file is marked as executable and fsrest is trying to execute it (and failing).

## Multiple Choices

If content negotiation (based on the request's `Accept` header) turns up multiple equally-good results, fsrest will respond with HTTP error code 300 ("Multiple Choices"). This is most likely to happen when a browser makes a request with `Accept: */*`. You can set a default representation for these situations by symlinking the preferred representation to `GET`.

# Building

I build fsrest with [Nix](http://nixos.org/nix/) to try to ensure reproducible builds:

```
nix-build dev.nix
```

`default.nix` is for inclusion in a top-level file (such as `all-packages.nix`). `dev.nix` builds fsrest with a fixed version of nixpkgs, providing stability at the cost of inflating the nix store.

# Installing on NixOS

`module.nix` provides a way for you to declaratively install and configure fsrest on a [NixOS](http://nixos.org/) system.
