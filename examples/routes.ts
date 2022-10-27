export {ParamValue, ResultValue, ReqOrUrl, Params, QueryStringOpts, Tokens, TokensA, HelperFunction, HelperFunction2, HelperFunction3, HelperFunctionN, Compact, RouteGen, RouteGenR, RoutePairs, Routes};

type ParamValue = string | number;

type ResultValue = string | URL;

type ReqOrUrl = URL | Request;

type Params<keys extends string> = Record<keys, ParamValue>;

type QueryStringOpts = ConstructorParameters<typeof URLSearchParams>[0];

type Tokens = ReadonlyArray<string>;

type TokensA = ReadonlyArray<Tokens>;

type HelperFunction<t extends string = never>
  = ([t] extends [never] ? HelperFunction2 : HelperFunction3<t>);

type HelperFunction2
  = (reqOrUrl: ReqOrURl, query: QueryStringOpts | undefined  => ResultValue);

type HelperFunction3<keys extends string>
  = (reqOrUrl: ReqOrURl, params: Params<keys>, query: QueryStringOpts
  | undefined  => ResultValue);

type HelperFunctionN = HelperFunction<any>;

type Compact<t> = {[k in keyof t]: t[k]};

type RouteGen<t extends readonly string[]>
  = (t extends readonly [infer hd extends string, ...infer tl]
    ? RouteGenR<tl, hd, never>
    : "error: expected type [string, ...any]");

type RouteGenR<t extends readonly Array<string>, args extends string>
  = (t extends [`:${infer paramName}`, ...infer tl]
    ? RouteGenR<tl, name, paramName | args>
    : (t extends [infer hd extends string, ...infer tl]
      ? RouteGenR<tl, `${name}${Capitalize<hd>}`, paramName | args>
      : [name, [args: {[k in args]: ParamValue}, query: Query | undefined]]));

type RoutePairs<t extends TokensA>
  = (t extends readonly (infer u)[]
    ? (u extends readonly string[]
      ? RouteGen<u>
      : "error: expected array of arrays of strings")
    : "error: expected array of arrays of strings");



type Routes<t extends ReadonlyArray<ReadonlyArray<string>>>
  = Compact<{[pair in RoutePairs<t> as `
  ${Pair[0]}
  Url`]: (Pair<[1]> extends unknown[]
    ? ( ...args: Pair[1] => URL)
    : "error: expected an array")} & {[pair in RoutePairs<t> as `
  ${Pair[0]}
  Path`]: (Pair<[1]> extends unknown[]
    ? ( ...args: Pair[1] => string)
    : "error: expected an array")}>;
