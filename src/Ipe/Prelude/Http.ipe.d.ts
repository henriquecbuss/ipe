import { JsonValue } from "./Json.ipe";

export type CreateAppOptions<TContext> = {
  port: number;
  createContext: (_: Record<never, never>) => Promise<TContext>;
  handleRequest: (
    context: TContext,
    request: IpeRequest
  ) => Promise<[Response, TContext]>;
};

export type IpeRequest = {
  endpoint: string;
  searchParams: Record<string, string>;
  body: JsonValue;
  headers: Record<string, string>;
  method: Method;
};

export type Method =
  | "GET"
  | "HEAD"
  | "POST"
  | "PUT"
  | "DELETE"
  | "CONNECT"
  | "OPTIONS"
  | "TRACE"
  | "PATCH";
