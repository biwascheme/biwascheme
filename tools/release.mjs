import child_process from "child_process";
import package_json from "../package.json"
const version = package_json["version"]
child_process.execSync(`git ci -m v${version}`)
child_process.execSync(`git tag v${version}`)
child_process.execSync(`git push origin master --tags`)
child_process.execSync(`npm publish`)
console.log("\nDone.")

