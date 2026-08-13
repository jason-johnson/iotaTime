import { readFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";

const scriptDirectory = path.dirname(new URL(import.meta.url).pathname);
const outputDirectory = path.resolve(process.argv[2] ?? "build/docs");
const publicModules = JSON.parse(await readFile(
  path.join(scriptDirectory, "public-modules.json"),
  "utf8",
));

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
for (const moduleName of publicModules) {
  const page = await readFile(
    path.join(outputDirectory, "docs", `${moduleName}.html`),
    "utf8",
  );
  if (page.includes('id="other-definitions"')) {
    throw new Error(`${moduleName}.html has ungrouped declarations`);
  }
}

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
for (const name of [
  "PatternRep",
  "LiteralPatternRep",
  "MkPattern",
  "MkLiteralPattern",
  "initialState",
  "finish",
  "parsePart",
  "formatPart",
  "literalText",
  "patternInitialState",
  "patternFinish",
  "patternParsePart",
  "patternFormatPart",
]) {
  if (patternPage.includes(`IotaTime.Pattern.${name}`)) {
    throw new Error(`IotaTime.Pattern.html exposes internal ${name}`);
  }
}

const durationPage = await readFile(
  path.join(outputDirectory, "docs", "IotaTime.Duration.html"),
  "utf8",
);
const hiddenDurationNames = [
  "durationFromNanoseconds",
  "durationFromMicroseconds",
  "durationFromMilliseconds",
  "durationFromSeconds",
  "durationFromMinutes",
  "durationFromHours",
  "durationFromStandardDays",
  "durationFromStandardWeeks",
  "addDurations",
  "subtractDurations",
];
for (const name of hiddenDurationNames) {
  if (durationPage.includes(name)) {
    throw new Error(`IotaTime.Duration.html exposes hidden helper ${name}`);
  }
}
if (!durationPage.includes(
  "Construct a duration from an exact number of nanoseconds.",
)) {
  throw new Error("IotaTime.Duration.html lacks the public fromNanoseconds description");
}

const offsetPage = await readFile(
  path.join(outputDirectory, "docs", "IotaTime.Offset.html"),
  "utf8",
);
const internalOffsetNames = [
  "OffsetRep",
  "totalOffsetSeconds",
  "offsetFromSeconds",
  "offsetFromMinutes",
  "offsetFromHours",
  "offsetHours",
  "offsetMinutes",
  "offsetSeconds",
  "zeroOffset",
  "addOffsetClamped",
  "subtractOffsetClamped",
];
for (const name of internalOffsetNames) {
  if (offsetPage.includes(`IotaTime.Offset.${name}`)) {
    throw new Error(`IotaTime.Offset.html exposes internal ${name}`);
  }
}
if (offsetPage.includes('id="other-definitions"')) {
  throw new Error("IotaTime.Offset.html has ungrouped declarations");
}

const calendarPage = await readFile(
  path.join(outputDirectory, "docs", "IotaTime.Calendar.html"),
  "utf8",
);
for (const name of [
  "DateDifferencePolicy.(.units)",
  "DateDifferencePolicy.(.monthArithmetic)",
]) {
  if (calendarPage.includes(`id="IotaTime.Calendar.${name}"`)) {
    throw new Error(`IotaTime.Calendar.html exposes duplicate projection ${name}`);
  }
}

const persianPage = await readFile(
  path.join(outputDirectory, "docs", "IotaTime.Calendar.Persian.html"),
  "utf8",
);
const hiddenPersianNames = [
  "weekdayFromDays",
  "epoch",
  "leapYears",
  "countLeapsBefore",
  "newYearDay",
  "lastDay",
  "monthOffset",
  "daysFromCivil",
  "nthDayOfMonth",
  "weekDateDays",
  "KnownPersianArithmeticRule",
  "arithmeticLastDay",
  "arithmeticDaysFromCivil",
];
for (const name of hiddenPersianNames) {
  if (persianPage.includes(`IotaTime.Calendar.Persian.${name}`)) {
    throw new Error(`IotaTime.Calendar.Persian.html exposes internal ${name}`);
  }
}
if (persianPage.includes('id="other-definitions"')) {
  throw new Error("IotaTime.Calendar.Persian.html has ungrouped declarations");
}

const timeZonePage = await readFile(
  path.join(outputDirectory, "docs", "IotaTime.TimeZone.html"),
  "utf8",
);
if (timeZonePage.includes("IotaTime.TimeZone.mappingCandidates")) {
  throw new Error("IotaTime.TimeZone.html exposes internal mappingCandidates");
}
for (const name of [
  "utc",
  "timeZone",
  "localZone",
  "availableZones",
  "metadata",
  "TimeZoneProvider",
  "timeZoneProvider",
  "systemTimeZoneProvider",
  "unixTimeZoneProvider",
  "windowsSnapshotTimeZoneProvider",
  "utcWith",
  "timeZoneWith",
  "localZoneWith",
  "availableZonesWith",
  "metadataWith",
  "TimeZoneCachePolicy",
  "timeZoneCachePolicy",
  "defaultTimeZoneCachePolicy",
  "cachedTimeZoneProvider",
]) {
  if (!timeZonePage.includes(`id="IotaTime.TimeZone.${name}"`)) {
    throw new Error(`IotaTime.TimeZone.html lacks public ${name}`);
  }
}
if (!timeZonePage.includes('href="IotaTime.ZonedDateTime.html"')) {
  throw new Error("IotaTime.TimeZone.html lacks zoned date-time link");
}

for (const moduleName of [
  "IotaTime.Tzdb",
  "IotaTime.Tzdb.Provider",
  "IotaTime.Tzdb.Metadata",
]) {
  try {
    await readFile(path.join(outputDirectory, "docs", `${moduleName}.html`));
    throw new Error(`${moduleName}.html exposes an internal support module`);
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
  }
}

console.log(`Validated documentation in ${outputDirectory}`);
