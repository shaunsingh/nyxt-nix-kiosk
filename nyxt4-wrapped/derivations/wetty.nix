{ lib
, stdenv
, fetchFromGitHub
, nodejs_23
, node-gyp
, python3
, pnpm_9
, makeWrapper
, dart-sass
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "wetty";
  version = "2.7.0";

  src = fetchFromGitHub {
    owner = "butlerx";
    repo = "wetty";
    rev = "643e88b78e5b692aecf878753fcc3867dc061786";
    sha256 = "sha256-YBXJA+B3NNp1gAKTP61gmi//gSxV65TMHQQ+qaxf+58=";
  };

  nativeBuildInputs = [
    nodejs_23
    node-gyp
    python3
    pnpm_9.configHook
    makeWrapper
    dart-sass
  ];

  pnpmDeps = pnpm_9.fetchDeps {
    inherit (finalAttrs) pname version src;
    hash = "sha256-zGJAXziQglF84nmmZ4a1+6MAqDHelPFFTpl5G6vdyiU=";
  };

  postPatch = ''
    # Skip husky prepare script.
    substituteInPlace package.json \
      --replace '"prepare": "husky install"' \
                '"prepare": "echo Skipping husky install"'

    # Patch socket.ts to avoid TS2742 (due to inferred Socket type).
    substituteInPlace src/client/wetty/socket.ts \
      --replace 'export const socket = io(' \
                'import { Socket } from "socket.io-client";
export const socket: Socket = io('
  '';

  configurePhase = ''
    runHook preConfigure
    export HOME=$(mktemp -d)
    pnpm config set node-linker hoisted
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild

    # Ensure that native modules are built from source.
    export npm_config_build_from_source=true

    # Perform an offline pnpm install.
    pnpm install --offline

    # Force rebuild the gc-stats native binding:
    # For each gc-stats package directory under the pnpm store, remove its build folder
    # (removing any prebuilt binary) and then run node-gyp rebuild.
    find node_modules/.pnpm -maxdepth 3 -type d -name "gc-stats" | while read -r dir; do
      echo "Forcing rebuild of gc-stats in $dir"
      rm -rf "$dir/build"
      (cd "$dir" && node-gyp rebuild)
    done

    # Link the dart-sass binary so that SCSS processing works.
    mkdir -p node_modules/.bin
    ln -sf ${dart-sass}/bin/sass node_modules/.bin/sass

    # Compile TypeScript, forcing the output into the "dist" directory.
    ./node_modules/.bin/tsc --outDir dist
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/node_modules/wetty
    mkdir -p $out/bin

    cp -r dist package.json node_modules $out/lib/node_modules/wetty/

    # Create a wrapper script to run the wetty server.
    makeWrapper ${nodejs_23}/bin/node $out/bin/wetty \
      --add-flags "$out/lib/node_modules/wetty/dist/main.js"
    runHook postInstall
  '';

  meta = with lib; {
    description = "Terminal in browser over HTTP/HTTPS";
    homepage = "https://github.com/butlerx/wetty";
    license = licenses.mit;
    mainProgram = "wetty";
    platforms = platforms.unix;
  };
})

