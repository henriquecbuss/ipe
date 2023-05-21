/**
 * @template TContext
 * @typedef {import('./Http.ipe').CreateAppOptions<TContext>} CreateAppOptions
 */

/**
 * @typedef {import('./Http.ipe').Method} Method
 */

/**
 * @typedef {import('./Json.ipe').JsonValue} JsonValue
 */

/**
 * @template TContext
 * @argument {CreateAppOptions<TContext>} options
 */
const createApp = async ({ port, createContext, handleRequest }) => {
  let context = await createContext({});

  Bun.serve({
    port,
    fetch: async (request) => {
      const url = new URL(request.url);
      const endpoint = url.pathname
        .split("/")
        .filter((segment) => segment.length > 0);

      /** @type {Map<string, string>} */
      const searchParams = new Map(url.searchParams);

      /** @type {Method} */
      const method = request.method;

      /** @type {JsonValue} */
      const body = await request.json();

      /** @type {Map<string, string>} */
      const headers = new Map(request.headers);

      const { response, newContext } = await handleRequest(context)({
        endpoint,
        searchParams,
        body,
        headers,
        method: [capitalize(method)],
      });

      try {
        return response;
      } finally {
        context = newContext;
      }
    },
  });
};

/**
 *
 * @param {JsonValue} value
 * @returns {Response}
 */
const jsonResponse = (value) => new Response(JSON.stringify(value));

/**
 *
 * @param {number} status
 * @returns {(response: Response) => Response}
 */
const withStatus = (status) => (response) => {
  response.status = status;
  return response;
};

/**
 *
 * @param {string} name
 * @returns {string}
 */
const capitalize = (name) =>
  name[0].toUpperCase() + name.slice(1).toLowerCase();

export default { createApp, jsonResponse, withStatus };
