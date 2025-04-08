{ lib
, stdenv
, fetchFromGitHub
, python310
, nodejs_22
, node-gyp
, pnpm_9
, makeWrapper
, dart-sass
}:

stdenv.mkDerivation rec {
  pname = "wetty";
  version = "2.7.0";

  src = fetchFromGitHub {
    owner = "butlerx";
    repo = "wetty";
    rev = "643e88b78e5b692aecf878753fcc3867dc061786";
    sha256 = "sha256-YBXJA+B3NNp1gAKTP61gmi//gSxV65TMHQQ+qaxf+58=";
  };

  nativeBuildInputs = [
    nodejs_22
    node-gyp
    python310
    pnpm_9.configHook
    makeWrapper
    dart-sass
  ];

  pnpmDeps = pnpm_9.fetchDeps {
    inherit pname version src;
    hash = "sha256-zGJAXziQglF84nmmZ4a1+6MAqDHelPFFTpl5G6vdyiU=";
  };

  postPatch = ''
    # no git so no need for husky
    substituteInPlace package.json --replace '"prepare": "husky install"' '"prepare": "echo Skipping husky install"'

    # fix build
    substituteInPlace src/client/wetty/socket.ts --replace 'export const socket = io(' 'import { Socket } from "socket.io-client";
export const socket: Socket = io('
  '';

  configurePhase = ''
    runHook preConfigure

    # fake home to sandbox
    export HOME=$(mktemp -d)
    pnpm config set node-linker hoisted

    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild

    export npm_config_build_from_source=true
    pnpm install --offline

    # rebuild native modules
    find node_modules/.pnpm -maxdepth 3 -type d -name gc-stats | while read -r dir; do
      echo "Rebuilding native module in $dir"
      (cd "$dir" && node-gyp rebuild)
    done
    find node_modules/.pnpm -maxdepth 3 -type d -name node-pty | while read -r dir; do
      echo "Rebuilding node-pty native module in $dir"
      (cd "$dir" && node-gyp rebuild)
    done

    # replace dart sass with nix package
    mkdir -p node_modules/.bin
    ln -sf ${dart-sass}/bin/sass node_modules/.bin/sass
    ./node_modules/.bin/tsc --outDir build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/node_modules/wetty
    mkdir -p $out/bin
    cp -r build package.json node_modules $out/lib/node_modules/wetty/
    mkdir -p $out/lib/node_modules/wetty/build/client
    cp -a src/assets/. $out/lib/node_modules/wetty/build/client/
    makeWrapper ${nodejs_22}/bin/node $out/bin/wetty --add-flags "$out/lib/node_modules/wetty/build/main.js"
    runHook postInstall
  '';

  meta = with lib; {
    description = "Terminal in browser over HTTP/HTTPS";
    homepage = "https://github.com/butlerx/wetty";
    license = licenses.mit;
    mainProgram = "wetty";
    platforms = platforms.unix;
  };
}

