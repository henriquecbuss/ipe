import { JsonValue } from "./Json.ipe";

export type CreateAppOptions<TContext> = {
  port: number;
  createContext: (_: Record<never, never>) => Promise<TContext>;
  handleRequest: (
    context: TContext
  ) => (
    request: IpeRequest
  ) => Promise<{ response: Response; newContext: TContext }>;
};

export type IpeRequest = {
  endpoint: string[];
  searchParams: Map<string, string>;
  body: JsonValue;
  headers: Map<string, string>;
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
