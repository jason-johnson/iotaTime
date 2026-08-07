import { readFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";

const outputDirectory = path.resolve(process.argv[2] ?? "build/docs");

function ruleProperties(stylesheet, selector) {
  const escaped = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const match = stylesheet.match(new RegExp(`${escaped}\\s*\\{([^}]*)\\}`));
  if (match === null) {
    throw new Error(`Missing CSS rule: ${selector}`);
  }

  return new Map(match[1]
    .split(";")
    .map((declaration) => declaration.trim())
    .filter(Boolean)
    .map((declaration) => {
      const separator = declaration.indexOf(":");
      return [
        declaration.slice(0, separator).trim(),
        declaration.slice(separator + 1).trim(),
      ];
    }));
}

function requireProperties(stylesheet, selector, expected) {
  const properties = ruleProperties(stylesheet, selector);
  for (const [name, value] of Object.entries(expected)) {
    if (properties.get(name) !== value) {
      throw new Error(
        `${selector} must set ${name}: ${value}; found ${properties.get(name) ?? "nothing"}`,
      );
    }
  }
}

const stylesheet = await readFile(
  path.join(outputDirectory, "iotatime.css"),
  "utf8",
);
await Promise.all([
  "cookbooks.html",
  "guide.html",
  "index.html",
].map((filename) => readFile(path.join(outputDirectory, filename), "utf8")));

const blockProperties = {
  display: "block",
  width: "100%",
  "max-width": "100%",
  "overflow-x": "auto",
  "box-sizing": "border-box",
  "white-space": "pre",
};

requireProperties(stylesheet, ".module-cookbook pre", blockProperties);
requireProperties(stylesheet, ".module-cookbook pre code", { display: "block" });
requireProperties(stylesheet, ".guide pre", blockProperties);
requireProperties(stylesheet, ".guide pre code", { display: "block" });

const patternPage = await readFile(
  path.join(outputDirectory, "docs", "IotaTime.Pattern.html"),
  "utf8",
);
if (!patternPage.includes('class="module-cookbook"')) {
  throw new Error("IotaTime.Pattern.html has no module cookbook");
}
if (!/<pre><code(?: class="[^"]+")?>[\s\S]*?<\/code><\/pre>/.test(patternPage)) {
  throw new Error("IotaTime.Pattern.html has no fenced cookbook code block");
}

console.log(`Validated documentation in ${outputDirectory}`);
