import child_process from "child_process";
import package_json from "../package.json" with { type: "json" };
const version = package_json["version"]
child_process.execSync("npm run build")
child_process.execSync(`cp release/biwascheme.js release/biwascheme-${version}.js`)
child_process.execSync(`cp release/biwascheme-min.js release/biwascheme-${version}-min.js`)
child_process.execSync(`git add release/biwascheme-${version}.js release/biwascheme-${version}-min.js`)
console.log("\nDone.")
