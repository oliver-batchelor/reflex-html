{ mkDerivation, base, bifunctors, bytestring, concurrent-supply
, containers, contravariant, data-default, dependent-map
, dependent-sum, dependent-sum-template, ghcjs-dom, lens, mtl
, reducers, reflex, reflex-transformers, semigroups, stateWriter
, stdenv, transformers
}:
mkDerivation {
  pname = "reflex-html";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors bytestring concurrent-supply containers
    contravariant data-default dependent-map dependent-sum
    dependent-sum-template ghcjs-dom lens mtl reducers reflex
    reflex-transformers semigroups stateWriter transformers
  ];
  executableHaskellDepends = [
    base containers mtl reflex transformers
  ];
  description = "HTML DSL for reflex-dom";
  license = stdenv.lib.licenses.bsd3;
}
