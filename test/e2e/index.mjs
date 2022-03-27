//
// Run tests with headless Chrome.
// Exit with 0 when all tests are passed and with 1 otherwise.
//

// Set this to `false` for debugging
const HEADLESS = true;

import { setTimeout } from 'timers/promises';
import puppeteer from 'puppeteer';

// Run test/spec.html.
// Show progress to stdout
// Returns if all tests are passed
const runUnitTest = async (page) => {
  await page.goto('http://localhost:7001/test/spec.html');
  while (true) {
    const progress = await page.$eval('#progress', (el) => el.innerHTML);
    const n_failures = await page.$eval('#total_failures', (el) => el.innerHTML);
    const n_errors = await page.$eval('#total_errors', (el) => el.innerHTML);
    console.log(`${progress}/100% ${n_failures}F${n_errors}E`);
    await setTimeout(1000);
    if (progress === "100") {
      return n_failures == 0 && n_errors == 0;
    }
  }
};

const browser = await puppeteer.launch({ headless: HEADLESS });
const page = await browser.newPage();
//await page.screenshot({ path: 'screenshot.png'});
const ok = await runUnitTest(page);
if (!ok) {
  process.exit(1);
}
await browser.close();
