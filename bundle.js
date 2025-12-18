import esbuild from "esbuild";

esbuild
  .build({
    entryPoints: ["output/Main/index.js"],
    bundle: true,
    platform: "node",
    outfile: "dist/app.cjs",
    format: "cjs",
    external: ["fsevents"],
    // target: "node16",
    banner: {
      js: "#!/usr/bin/env node\n(function() {"
    },
    footer: {
      js: "\nconst m = module.exports.main; m(); })();"
    }
  })
  .catch((err) => {
    console.error(err);
    process.exit(1);
  });
