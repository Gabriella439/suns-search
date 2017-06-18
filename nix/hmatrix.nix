{ mkDerivation, array, base, binary, blas, bytestring, deepseq
, liblapack, random, split, stdenv, storable-complex, vector
, fetchFromGitHub, openblasCompat
}:
mkDerivation {
  pname = "hmatrix";
  version = "0.18.0.0";
  src =
    let
      repo = fetchFromGitHub {
        owner = "nh2";

        repo = "hmatrix";

        rev = "bd722cbf3da9d290d0b6d6fc3c900efb914b417f";

        sha256 = "0lp9g7kizkc78c3amj5sgkhiw8ym043x455z81whqk1zc7vl6xpk";
      };

    in
      "${repo}/packages/base";
  configureFlags = [ "-fopenblas" "-fdisable-default-paths" ];
  libraryHaskellDepends = [
    array base binary bytestring deepseq random split storable-complex
    vector
  ];
  librarySystemDepends = [ openblasCompat ];
  preConfigure = "sed -i hmatrix.cabal -e 's@/usr/@/dont/hardcode/paths/@'";
  homepage = "https://github.com/albertoruiz/hmatrix";
  description = "Numeric Linear Algebra";
  license = stdenv.lib.licenses.bsd3;
}
