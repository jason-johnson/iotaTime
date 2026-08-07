import { access, cp, mkdir, readFile, readdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { load } from "cheerio";
import { marked } from "marked";

const scriptDirectory = path.dirname(new URL(import.meta.url).pathname);
const outputDirectory = path.resolve(process.argv[2] ?? "build/docs");
const docsDirectory = path.join(outputDirectory, "docs");
const cookbooksDirectory = path.join(scriptDirectory, "cookbooks");
const groups = JSON.parse(
  await readFile(path.join(scriptDirectory, "groups.json"), "utf8"),
);
const publicModules = new Set(JSON.parse(
  await readFile(path.join(scriptDirectory, "public-modules.json"), "utf8"),
));

function slugify(value) {
  return value
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
}

function declarationName(moduleName, id) {
  const prefix = `${moduleName}.`;
  return id.startsWith(prefix)
    ? id
        .slice(prefix.length)
        .replaceAll("&apos;", "'")
        .replaceAll("&#39;", "'")
    : null;
}

function addSharedNavigation($, root) {
  const nav = $("header nav").first();
  if (nav.length > 0 && nav.find(".guide-link").length === 0) {
    nav.prepend(`<a class="guide-link" href="${root}guide.html">Guide</a>`);
  }

  if ($('link[href$="iotatime.css"]').length === 0) {
    $("head").append(
      `<link rel="stylesheet" type="text/css" href="${root}iotatime.css">`,
    );
  }
}

function groupModulePage(html, moduleName, configuredGroups, cookbook) {
  const $ = load(html, { decodeEntities: false });
  addSharedNavigation($, "../");

  if (cookbook !== undefined) {
    $("#module-header").after(
      `<section class="module-cookbook">${marked.parse(cookbook)}</section>`,
    );
  }

  $("dl.decls > dt").each((_, element) => {
    const term = $(element);
    const description = term.next("dd");
    const visibility = description.find("b")
      .filter((__, label) => $(label).text().trim() === "Visibility")
      .next("span.keyword")
      .text()
      .replaceAll(" ", " ")
      .trim();
    if (visibility !== "public export") {
      description.remove();
      term.remove();
    }
  });

  const definitionsHeading = $("h2")
    .filter((_, element) => $(element).text().trim() === "Definitions")
    .first();
  const definitions = definitionsHeading.next("dl.decls");
  if (definitions.length === 0) return $.html();

  const entries = [];
  definitions.children("dt").each((_, element) => {
    const term = $(element);
    const description = term.next("dd");
    entries.push({
      name: declarationName(moduleName, term.attr("id") ?? ""),
      term,
      description: description.length > 0 ? description : null,
    });
  });

  const assigned = new Set();
  const sections = [];
  for (const group of configuredGroups) {
    const names = new Set(group.names);
    const matching = entries.filter(
      (entry) => entry.name !== null && names.has(entry.name),
    );
    if (matching.length === 0) continue;
    matching.forEach((entry) => assigned.add(entry));
    sections.push({ title: group.title, entries: matching });
  }

  const remaining = entries.filter((entry) => !assigned.has(entry));
  if (remaining.length > 0) {
    sections.push({ title: "Other definitions", entries: remaining });
  }

  const contents = $('<nav class="api-toc"><strong>On this page</strong><ul></ul></nav>');
  const renderedSections = [];
  for (const section of sections) {
    const id = slugify(section.title);
    contents.find("ul").append(`<li><a href="#${id}">${section.title}</a></li>`);
    const rendered = $(`<section class="api-group"><h2 id="${id}">${section.title}</h2><dl class="decls"></dl></section>`);
    const list = rendered.children("dl");
    for (const entry of section.entries) {
      list.append(entry.term);
      if (entry.description !== null) list.append(entry.description);
    }
    renderedSections.push(rendered);
  }

  definitionsHeading.replaceWith(contents);
  definitions.replaceWith(renderedSections);

  const renderedEntryCount = $("section.api-group > dl.decls > dt").length;
  if (renderedEntryCount !== entries.length) {
    throw new Error(
      `${moduleName}: grouped ${renderedEntryCount} of ${entries.length} definitions`,
    );
  }
  return $.html();
}

function enhanceIndex(html) {
  const $ = load(html, { decodeEntities: false });
  addSharedNavigation($, "");
  $(".index-namespace-url > a.code").each((_, element) => {
    if (!publicModules.has($(element).text().trim())) {
      $(element).closest("li").remove();
    }
  });
  const heading = $(".container > h1").first();
  heading.replaceWith(`
    <section class="docs-hero">
      <h1>iotaTime</h1>
      <p>Proof-oriented date, time, calendar, locale, and timezone APIs for Idris 2.</p>
    </section>
    <div class="docs-callout">
      Start with the <a href="guide.html">iotaTime guide</a> for concepts and compiled examples,
      then use the module reference below for complete signatures.
    </div>
  `);
  return $.html();
}

function guidePage(markdown) {
  const body = marked.parse(markdown);
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>iotaTime Guide</title>
  <link rel="stylesheet" type="text/css" href="default.css">
  <link rel="stylesheet" type="text/css" href="iotatime.css">
</head>
<body>
<header>
  <strong>iotaTime</strong>
  <nav><a class="guide-link" href="guide.html">Guide</a><a href="index.html">API index</a></nav>
</header>
<main class="container guide">${body}</main>
</body>
</html>`;
}

async function validateLocalLinks(filePath) {
  const $ = load(await readFile(filePath, "utf8"), { decodeEntities: false });
  for (const element of $("a[href]").toArray()) {
    const href = $(element).attr("href");
    if (
      href === undefined ||
      href.startsWith("#") ||
      /^[a-z]+:/i.test(href)
    ) {
      continue;
    }
    const target = decodeURIComponent(href.split(/[?#]/, 1)[0]);
    try {
      await access(path.resolve(path.dirname(filePath), target));
    } catch {
      throw new Error(`${path.basename(filePath)}: broken link ${href}`);
    }
  }
}

await mkdir(outputDirectory, { recursive: true });
await cp(path.join(scriptDirectory, "iotatime.css"), path.join(outputDirectory, "iotatime.css"));

const cookbooks = new Map();
for (const filename of await readdir(cookbooksDirectory)) {
  if (!filename.endsWith(".md")) continue;
  const moduleName = filename.slice(0, -3);
  if (!publicModules.has(moduleName)) {
    throw new Error(`${filename}: cookbook module is not public`);
  }
  cookbooks.set(
    moduleName,
    await readFile(path.join(cookbooksDirectory, filename), "utf8"),
  );
}

const indexPath = path.join(outputDirectory, "index.html");
await writeFile(indexPath, enhanceIndex(await readFile(indexPath, "utf8")));

for (const [moduleName, configuredGroups] of Object.entries(groups)) {
  const modulePath = path.join(docsDirectory, `${moduleName}.html`);
  const html = await readFile(modulePath, "utf8");
  await writeFile(modulePath, groupModulePage(
    html,
    moduleName,
    configuredGroups,
    cookbooks.get(moduleName),
  ));
}

const moduleFiles = await readdir(docsDirectory);
for (const filename of moduleFiles.filter((value) => value.endsWith(".html"))) {
  const moduleName = filename.slice(0, -5);
  if (!publicModules.has(moduleName)) {
    await rm(path.join(docsDirectory, filename));
    continue;
  }
  if (Object.hasOwn(groups, moduleName)) continue;
  const modulePath = path.join(docsDirectory, filename);
  await writeFile(modulePath, groupModulePage(
    await readFile(modulePath, "utf8"),
    moduleName,
    [],
    cookbooks.get(moduleName),
  ));
}

const guide = await readFile(path.join(scriptDirectory, "guide.md"), "utf8");
const guidePath = path.join(outputDirectory, "guide.html");
await writeFile(guidePath, guidePage(guide));

await validateLocalLinks(indexPath);
await validateLocalLinks(guidePath);

console.log(`Enhanced documentation in ${outputDirectory}`);
