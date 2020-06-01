{ mkDerivation, aeson, attoparsec, base, bytestring, case-insensitive, deepseq, directory, filepath, Glob, http-types, network, network-uri, parsec, process, stdenv, text, transformers, unix, utf8-string, vector, wai, warp }:
mkDerivation {
  pname = "fsrest";
  version = "0.6.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ aeson attoparsec base bytestring case-insensitive deepseq directory filepath Glob http-types network network-uri parsec process text transformers unix utf8-string vector wai warp ];
  homepage = "https://github.com/jekor/fsrest";
  description = "filesystem-based RESTful HTTP server";
  license = stdenv.lib.licenses.mit;
}
