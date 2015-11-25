{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, directory, filepath, Glob, http-kit, http-types, network
, network-uri, process, stdenv, text, transformers, unix
, utf8-string
}:
mkDerivation {
  pname = "fsrest";
  version = "0.3.4";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring case-insensitive directory filepath Glob
    http-kit http-types network network-uri process text transformers
    unix utf8-string
  ];
  homepage = "https://github.com/jekor/fsrest";
  description = "filesystem-based RESTful HTTP server";
  license = stdenv.lib.licenses.mit;
}
