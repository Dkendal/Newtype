export type Equals<A1 extends any, A2 extends any> =
  (<A>() => A extends A2 ? 1 : 0) extends <A>() => A extends A1 ? 1 : 0 ? 1 : 0;

declare function check<Type, Expect, Outcome extends 0 | 1>(
  debug?: Type,
): Equals<Equals<Type, Expect>, Outcome>;

declare function checks(checks: 1[]): void;

type Extends<A, B> = A extends B ? true : false;

type Pass = 1;
type Fail = 0;

{
  type t = any;

  checks([
    check<Extends<t , t>             , true  , Pass>() ,
    check<Extends<t , any>           , true    , Pass>() ,
    check<Extends<t , unknown>       , true    , Pass>() ,
    check<Extends<t , {}>            , boolean , Pass>() ,
    check<Extends<t , { x: string }> , boolean , Pass>() ,
    check<Extends<t , symbol>        , boolean , Pass>() ,
    check<Extends<t , null>          , boolean , Pass>() ,
    check<Extends<t , undefined>     , boolean , Pass>() ,
    check<Extends<t , void>          , boolean , Pass>() ,
    check<Extends<t , number>        , boolean , Pass>() ,
    check<Extends<t , string>        , boolean , Pass>() ,
    check<Extends<t , boolean>       , boolean , Pass>() ,
    check<Extends<t , bigint>        , boolean , Pass>() ,
    check<Extends<t , symbol>        , boolean , Pass>() ,
    check<Extends<t , object>        , boolean , Pass>() ,
    check<Extends<t , Object>        , boolean , Pass>() ,
    check<Extends<t , Function>      , boolean , Pass>() ,
    check<Extends<t , String>        , boolean , Pass>() ,
    check<Extends<t , []>            , boolean , Pass>() ,
    check<Extends<t , [t]>           , boolean , Pass>() ,
    check<Extends<t , t[]>           , boolean , Pass>() ,
    check<Extends<t , 1>             , boolean , Pass>() ,
    check<Extends<t , "">            , boolean , Pass>() ,
    check<Extends<t , true>          , boolean , Pass>() ,
    check<Extends<t , false>         , boolean , Pass>() ,
    check<Extends<t , never>         , boolean , Pass>() ,
  ]);
}

{
  type t = unknown;
  checks([
    check<Extends<t , t>             , true  , Pass>() ,
    check<Extends<t , any>           , true  , Pass>() ,
    check<Extends<t , unknown>       , true  , Pass>() ,
    check<Extends<t , {}>            , false , Pass>() ,
    check<Extends<t , { x: string }> , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , null>          , false , Pass>() ,
    check<Extends<t , undefined>     , false , Pass>() ,
    check<Extends<t , void>          , false , Pass>() ,
    check<Extends<t , number>        , false , Pass>() ,
    check<Extends<t , string>        , false , Pass>() ,
    check<Extends<t , boolean>       , false , Pass>() ,
    check<Extends<t , bigint>        , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , object>        , false , Pass>() ,
    check<Extends<t , Object>        , false , Pass>() ,
    check<Extends<t , Function>      , false , Pass>() ,
    check<Extends<t , String>        , false , Pass>() ,
    check<Extends<t , () => void>    , false , Pass>() ,
    check<Extends<t , []>            , false , Pass>() ,
    check<Extends<t , [t]>           , false , Pass>() ,
    check<Extends<t , t[]>           , false , Pass>() ,
    check<Extends<t , 1>             , false , Pass>() ,
    check<Extends<t , "">            , false , Pass>() ,
    check<Extends<t , true>          , false , Pass>() ,
    check<Extends<t , false>         , false , Pass>() ,
    check<Extends<t , never>         , false , Pass>() ,
  ]);
}

{
  type t = {};
  checks([
    check<Extends<t , t>             , true  , Pass>() ,
    check<Extends<t , any>           , true  , Pass>() ,
    check<Extends<t , unknown>       , true  , Pass>() ,
    check<Extends<t , {}>            , true  , Pass>() ,
    check<Extends<t , { x: string }> , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , null>          , false , Pass>() ,
    check<Extends<t , undefined>     , false , Pass>() ,
    check<Extends<t , void>          , false , Pass>() ,
    check<Extends<t , number>        , false , Pass>() ,
    check<Extends<t , string>        , false , Pass>() ,
    check<Extends<t , boolean>       , false , Pass>() ,
    check<Extends<t , bigint>        , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , object>        , true  , Pass>() ,
    check<Extends<t , Object>        , true  , Pass>() ,
    check<Extends<t , Function>      , false , Pass>() ,
    check<Extends<t , String>        , false , Pass>() ,
    check<Extends<t , () => void>    , false , Pass>() ,
    check<Extends<t , []>            , false , Pass>() ,
    check<Extends<t , [t]>           , false , Pass>() ,
    check<Extends<t , t[]>           , false , Pass>() ,
    check<Extends<t , 1>             , false , Pass>() ,
    check<Extends<t , "">            , false , Pass>() ,
    check<Extends<t , true>          , false , Pass>() ,
    check<Extends<t , false>         , false , Pass>() ,
    check<Extends<t , never>         , false , Pass>() ,
  ]);
}

{
  type t = "string";
  checks([
    check<Extends<t , t>             , true  , Pass>() ,
    check<Extends<t , any>           , true  , Pass>() ,
    check<Extends<t , unknown>       , true  , Pass>() ,
    check<Extends<t , {}>            , true  , Pass>() ,
    check<Extends<t , { x: string }> , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , null>          , false , Pass>() ,
    check<Extends<t , undefined>     , false , Pass>() ,
    check<Extends<t , void>          , false , Pass>() ,
    check<Extends<t , number>        , false , Pass>() ,
    check<Extends<t , string>        , true  , Pass>() ,
    check<Extends<t , boolean>       , false , Pass>() ,
    check<Extends<t , bigint>        , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , object>        , false , Pass>() ,
    check<Extends<t , Object>        , true  , Pass>() ,
    check<Extends<t , Function>      , false , Pass>() ,
    check<Extends<t , String>        , true  , Pass>() ,
    check<Extends<t , () => void>    , false , Pass>() ,
    check<Extends<t , []>            , false , Pass>() ,
    check<Extends<t , [t]>           , false , Pass>() ,
    check<Extends<t , t[]>           , false , Pass>() ,
    check<Extends<t , 1>             , false , Pass>() ,
    check<Extends<t , "">            , false , Pass>() ,
    check<Extends<t , true>          , false , Pass>() ,
    check<Extends<t , false>         , false , Pass>() ,
    check<Extends<t , never>         , false , Pass>() ,
  ]);
}

{
  type t = "String";
  checks([
    check<Extends<t , t>             , true  , Pass>() ,
    check<Extends<t , any>           , true  , Pass>() ,
    check<Extends<t , unknown>       , true  , Pass>() ,
    check<Extends<t , {}>            , true  , Pass>() ,
    check<Extends<t , { x: string }> , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , null>          , false , Pass>() ,
    check<Extends<t , undefined>     , false , Pass>() ,
    check<Extends<t , void>          , false , Pass>() ,
    check<Extends<t , number>        , false , Pass>() ,
    check<Extends<t , string>        , true  , Pass>() ,
    check<Extends<t , boolean>       , false , Pass>() ,
    check<Extends<t , bigint>        , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , object>        , false , Pass>() ,
    check<Extends<t , Object>        , true  , Pass>() ,
    check<Extends<t , Function>      , false , Pass>() ,
    check<Extends<t , String>        , true  , Pass>() ,
    check<Extends<t , () => void>    , false , Pass>() ,
    check<Extends<t , []>            , false , Pass>() ,
    check<Extends<t , [t]>           , false , Pass>() ,
    check<Extends<t , t[]>           , false , Pass>() ,
    check<Extends<t , 1>             , false , Pass>() ,
    check<Extends<t , "">            , false , Pass>() ,
    check<Extends<t , true>          , false , Pass>() ,
    check<Extends<t , false>         , false , Pass>() ,
    check<Extends<t , never>         , false , Pass>() ,
  ]);
}

{
  type t = "string";
  checks([
    check<Extends<t , t>             , true  , Pass>() ,
    check<Extends<t , any>           , true  , Pass>() ,
    check<Extends<t , unknown>       , true  , Pass>() ,
    check<Extends<t , {}>            , true  , Pass>() ,
    check<Extends<t , { x: string }> , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , null>          , false , Pass>() ,
    check<Extends<t , undefined>     , false , Pass>() ,
    check<Extends<t , void>          , false , Pass>() ,
    check<Extends<t , number>        , false , Pass>() ,
    check<Extends<t , string>        , true  , Pass>() ,
    check<Extends<t , boolean>       , false , Pass>() ,
    check<Extends<t , bigint>        , false , Pass>() ,
    check<Extends<t , symbol>        , false , Pass>() ,
    check<Extends<t , object>        , false , Pass>() ,
    check<Extends<t , Object>        , true  , Pass>() ,
    check<Extends<t , Function>      , false , Pass>() ,
    check<Extends<t , String>        , true  , Pass>() ,
    check<Extends<t , () => void>    , false , Pass>() ,
    check<Extends<t , []>            , false , Pass>() ,
    check<Extends<t , [t]>           , false , Pass>() ,
    check<Extends<t , t[]>           , false , Pass>() ,
    check<Extends<t , 1>             , false , Pass>() ,
    check<Extends<t , "">            , false , Pass>() ,
    check<Extends<t , true>          , false , Pass>() ,
    check<Extends<t , false>         , false , Pass>() ,
    check<Extends<t , never>         , false , Pass>() ,
  ]);
}

{
  type t = never;
  checks([
    check<Extends<t , t>             , never , Pass>() ,
    check<Extends<t , any>           , never , Pass>() ,
    check<Extends<t , unknown>       , never , Pass>() ,
    check<Extends<t , {}>            , never , Pass>() ,
    check<Extends<t , { x: string }> , never , Pass>() ,
    check<Extends<t , symbol>        , never , Pass>() ,
    check<Extends<t , null>          , never , Pass>() ,
    check<Extends<t , undefined>     , never , Pass>() ,
    check<Extends<t , void>          , never , Pass>() ,
    check<Extends<t , number>        , never , Pass>() ,
    check<Extends<t , string>        , never , Pass>() ,
    check<Extends<t , boolean>       , never , Pass>() ,
    check<Extends<t , bigint>        , never , Pass>() ,
    check<Extends<t , symbol>        , never , Pass>() ,
    check<Extends<t , object>        , never , Pass>() ,
    check<Extends<t , Object>        , never , Pass>() ,
    check<Extends<t , Function>      , never , Pass>() ,
    check<Extends<t , String>        , never , Pass>() ,
    check<Extends<t , () => void>    , never , Pass>() ,
    check<Extends<t , []>            , never , Pass>() ,
    check<Extends<t , [t]>           , never , Pass>() ,
    check<Extends<t , t[]>           , never , Pass>() ,
    check<Extends<t , 1>             , never , Pass>() ,
    check<Extends<t , "">            , never , Pass>() ,
    check<Extends<t , true>          , never , Pass>() ,
    check<Extends<t , false>         , never , Pass>() ,
    check<Extends<t , never>         , never , Pass>() ,
  ]);
}


// Primitives do not extend any other primitives
{
  checks([
    check<Extends<string , string>             , true  , Pass>() ,
    check<Extends<string , number>             , false  , Pass>() ,
    check<Extends<string , boolean>            , false  , Pass>() ,
    check<Extends<string , bigint>             , false  , Pass>() ,
    check<Extends<string , symbol>             , false  , Pass>() ,
    check<Extends<string , object>             , false  , Pass>() ,
  ]);

  checks([
    check<Extends<object , string>             , false  , Pass>() ,
    check<Extends<object , number>             , false  , Pass>() ,
    check<Extends<object , boolean>            , false  , Pass>() ,
    check<Extends<object , bigint>             , false  , Pass>() ,
    check<Extends<object , symbol>             , false  , Pass>() ,
    check<Extends<object , object>             , true  , Pass>() ,
  ]);

  checks([
    check<Extends<number , string>             , false  , Pass>() ,
    check<Extends<number , number>             , true  , Pass>() ,
    check<Extends<number , boolean>            , false  , Pass>() ,
    check<Extends<number , bigint>             , false  , Pass>() ,
    check<Extends<number , symbol>             , false  , Pass>() ,
    check<Extends<number , object>             , false  , Pass>() ,
  ]);

  checks([
    check<Extends<bigint , string>             , false  , Pass>() ,
    check<Extends<bigint , number>             , false  , Pass>() ,
    check<Extends<bigint , boolean>            , false  , Pass>() ,
    check<Extends<bigint , bigint>             , true  , Pass>() ,
    check<Extends<bigint , symbol>             , false  , Pass>() ,
    check<Extends<bigint , object>             , false  , Pass>() ,
  ]);
}

{
  checks([
    check<Extends<number , null | undefined> , false , Pass>() ,
    check<Extends<string , null | undefined> , false , Pass>() ,
    check<Extends<1 , null | undefined> , false , Pass>() ,
    check<Extends<'' , null | undefined> , false , Pass>() ,
  ]);
}

{
  checks([
    check<Extends<number    , Object> , true  , Pass>() ,
    check<Extends<string    , Object> , true  , Pass>() ,
    check<Extends<1         , Object> , true  , Pass>() ,
    check<Extends<{}        , Object> , true  , Pass>() ,
    check<Extends<false     , Object> , true  , Pass>() ,
    check<Extends<void      , Object> , false , Pass>() ,
    check<Extends<undefined , Object> , false , Pass>() ,
    check<Extends<null      , Object> , false , Pass>() ,
  ]);

  checks([
    check<Extends<number    , object> , false , Pass>() ,
    check<Extends<string    , object> , false , Pass>() ,
    check<Extends<1         , object> , false , Pass>() ,
    check<Extends<{}        , object> , true  , Pass>() ,
    check<Extends<false     , object> , false , Pass>() ,
    check<Extends<void      , object> , false , Pass>() ,
    check<Extends<undefined , object> , false , Pass>() ,
    check<Extends<null      , object> , false , Pass>() ,
  ]);
}
