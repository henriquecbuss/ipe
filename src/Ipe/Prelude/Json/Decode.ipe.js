/**
 * @typedef {import("../Json.ipe").JsonValue} JsonValue
 */

/**
 * @template T
 * @typedef {{ ok: true; parsed: T } | { ok: false; error: string }} ParseResult
 */

/**
 * @template T
 * @typedef {(input: JsonValue) => ParseResult<T>} Parser
 */

/**
 * @template T
 * @param {Parser<T>} parser
 * @param {JsonValue} input
 * @returns {ParseResult<T>}
 */
const parseJsonValue = (parser, input) => {
  return parser(input);
};

/**
 * @template T
 * @param {Parser<T>} parser
 * @param {string} input
 * @returns {ParseResult<T>}
 */
const parseString = (parser, input) => {
  try {
    /** @type {JsonValue} */
    const parsed = JSON.parse(input);
    return parseJsonValue(parser, parsed);
  } catch (error) {
    return { ok: false, error: `Invalid JSON: ${error}` };
  }
};

/**
 * @type {Parser<string>}
 */
const string = (input) => {
  if (typeof input === "string") {
    return { ok: true, parsed: input };
  }

  return { ok: false, error: `Expected string, but got ${typeof input}` };
};

/**
 * @type {Parser<number>}
 */
const number = (input) => {
  if (typeof input === "number") {
    return { ok: true, parsed: input };
  }

  return { ok: false, error: `Expected number, but got ${typeof input}` };
};

/**
 * @type {Parser<boolean>}
 */
const bool = (input) => {
  if (typeof input === "boolean") {
    return { ok: true, parsed: input };
  }

  return { ok: false, error: `Expected boolean, but got ${typeof input}` };
};

/**
 * @type {Parser<null>}
 */
const nothing = (input) => {
  if (input === null) {
    return { ok: true, parsed: input };
  }

  return { ok: false, error: `Expected null, but got ${typeof input}` };
};

/**
 * @template T
 * @param {Parser<T>} itemParser
 * @returns {Parser<T[]>}
 */
const list = (itemParser) => {
  return (input) => {
    if (!Array.isArray(input)) {
      return { ok: false, error: `Expected list, but got ${typeof input}` };
    }

    /** @type {T[]} */
    const result = [];
    for (const item of input) {
      const itemResult = itemParser(item);

      if (!itemResult.ok) {
        return itemResult;
      }

      result.push(itemResult.parsed);
    }

    return { ok: true, parsed: result };
  };
};

/**
 * @template T
 * @typedef {{[K in keyof T]: T[K] extends Parser<infer U> ? Parser<U> : never;}} ObjectParserInput
 */

/**
 * @template T
 * @typedef {{[K in keyof T]: T[K] extends Parser<infer U> ? U : never;}} ObjectParserOutput
 */

/**
 * @template  T
 * @param {ObjectParserInput<T>} fieldParsers
 * @returns {Parser<ObjectParserOutput<T>>}
 */
const object = (fieldParsers) => {
  return (input) => {
    if (typeof input !== "object" || input === null || Array.isArray(input)) {
      return {
        ok: false,
        error: `Expected an object, but got ${typeof input}`,
      };
    }

    /** @type {Partial<ObjectParserOutput<T>>} */
    const parsed = {};

    for (const stringKey of Object.keys(fieldParsers)) {
      const key = /** @type {keyof T} */ (stringKey);
      const fieldParser = fieldParsers[key];

      if (!(stringKey in input)) {
        return { ok: false, error: `field ${stringKey} is missing` };
      }

      const fieldValue = /** @type {JsonValue} */ (input[stringKey]);

      const fieldResult = fieldParser(fieldValue);

      if (!fieldResult.ok) {
        // return fieldResult;
        return {
          ok: false,
          error: `Error in field ${stringKey}: ${fieldResult.error}`,
        };
      }

      const parsedValue = fieldResult.parsed;

      // @ts-ignore
      parsed[key] = parsedValue;
    }

    const result = /** @type {ObjectParserOutput<T>} */ (parsed);

    return { ok: true, parsed: result };
  };
};

/**
 * @template T
 * @param {Parser<T>[]} parsers
 * @returns {Parser<T>}
 */
const oneOf = (parsers) => {
  return (input) => {
    /** @type {string[]} */
    const errors = [];
    for (const parser of parsers) {
      const result = parser(input);
      if (result.ok) {
        return result;
      }

      errors.push(result.error);
    }

    return { ok: false, error: `No parser matched:\n${errors.join("\n")}` };
  };
};

export default {
  parseJsonValue,
  parseString,
  string,
  number,
  bool,
  nothing,
  list,
  object,
  oneOf,
};
