with import <nixpkgs> {};

stdenv.mkDerivation {
    name = "node";
    buildInputs = [
        nodejs-16_x
    ];
    shellHook = ''
        export PATH="$PWD/node_modules/.bin/:$PATH"
        export BROWSER="/usr/bin/firefox"
        alias scripts='jq ".scripts" package.json'
        alias run='npm run'
    '';
}