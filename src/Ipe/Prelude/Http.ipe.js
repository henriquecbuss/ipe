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

      const searchParams = Object.fromEntries(url.searchParams.entries());

      /** @type {Method} */
      const method = request.method;

      /** @type {JsonValue} */
      const body = await request.json();

      const headers = Object.fromEntries(request.headers.entries());

      const [response, newContext] = await handleRequest(context, {
        endpoint,
        searchParams,
        body,
        headers,
        method,
      });

      try {
        return response;
      } finally {
        context = newContext;
      }
    },
  });
};

export default { createApp };
