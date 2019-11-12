let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20191005/packages.dhall sha256:ba287d858ada09c4164792ad4e643013b742c208cbedf5de2e35ee27b64b6817

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
