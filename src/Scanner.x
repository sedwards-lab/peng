{

-- https://stackoverflow.com/questions/20315739/how-to-use-an-alex-monadic-lexer-with-happy

module Scanner where
}

%wrapper "monadUserState"

$digit   = 0-9
$blank   = [\ \t]
@newline = [\n] | [\r][\n] | [\r]
@identifier = [a-zA-Z] [a-zA-Z0-9_']*

tokens :-

  $blank+ ; -- Whitespace
  
  "//".*  ; -- Single-line comments; always just ignore them

  <startBlock> {
    @newline    ;
    ()          { firstBlockToken } -- First token in a new block
  }

  <startLine> {
    @newline    ;
    ()          { firstLineToken } -- First token on the line
  }

  <freeform> {
    @newline    ;
  }

  <0,inBlock> {
    @newline { nextLine }
  }

  <0,freeform,inBlock> {
    
    if    { layoutNL TIf }
    else  { layout   TElse }
    while { layoutNL TWhile }
    let   { layout   TLet }
    case  { layoutNL TCase }
    \=    { layout   TEq }
    do    { doBlock }

    \;    { keyword TSemicolon }

    \(    { lDelimeter TLparen }
    \)    { rDelimeter TRparen }
    \[    { lDelimeter TLbracket }
    \]    { rDelimeter TRbracket }
    \{    { lDelimeter TLbrace }
    \}    { rDelimeter TRbrace }
  
    $digit+ { \ (pos,_,_,s) len ->
                  return $ Token pos $ TInteger $ read $ take len s }

    @identifier { \ (pos,_,_,s) len -> return $ Token pos $ TId $ take len s }
  }
  
{

-- Get the position of the current token from an AlexInput
inputPos :: AlexInput -> AlexPosn
inputPos (pos, _, _, _) = pos

-- Get the column of the current token
inputCol :: AlexInput -> Int
inputCol ((AlexPn _ _ col), _, _, _) = col

-- Plain keyword helper: lexeme is discarded
keyword :: TokenType -> AlexInput -> Int -> Alex Token
keyword ttype alexInput _ = return $ Token (inputPos alexInput) ttype

-- Layout keyword: 
layout :: TokenType -> AlexInput -> Int -> Alex Token
layout ttype alexInput _ = do alexPushContext StartBlock
                              return $ Token (inputPos alexInput) ttype

-- Layout-next-line keyword: set to StartBlockNL state
layoutNL :: TokenType -> AlexInput -> Int -> Alex Token
layoutNL ttype alexInput _ = do
   alexPushContext StartBlockNL
   return $ Token (inputPos alexInput) ttype

lDelimeter :: TokenType -> AlexInput -> Int -> Alex Token
lDelimeter ttype alexInput _ = do
  case ttype of
    TLbrace -> do
      ctxt <- alexPeekContext
      case ctxt of
        StartBlock   -> alexPopContext -- About to start a block? Found it
        StartBlockNL -> alexPopContext -- About to start a block? Found it
        _            -> return ()
    _ -> return ()
  alexPushContext Freeform
  return $ Token (inputPos alexInput) ttype

rDelimeter :: TokenType -> AlexInput -> Int -> Alex Token
rDelimeter ttype alexInput _ = do alexPopContext
                                  alexPeekContext >>= alexSwitchContext
                                  return $ Token (inputPos alexInput) ttype

-- Immediately start a block
doBlock :: AlexInput -> Int -> Alex Token
doBlock _ _ = do alexPushContext StartBlock
                 alexMonadScan


data ScannerContext = InBlock Int  -- In a block with the given left margin
                    | StartBlock   -- Start a block at the next token
                    | StartBlockNL -- Start a block on the next line
                    | Freeform     -- Outside a block; ignore indentation

data AlexUserState = AlexUserState { usContext :: [ScannerContext]
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { usContext = [InBlock 1]
                                  }

{-
alexGetContext :: Alex [ScannerContext]
alexGetContext = usContext <$> alexGetUserState

alexSetContext :: [ScannerContext] -> Alex ()
alexSetContext ctxt = do st <- alexGetUserState
                         alexSetUserState $ st { usContext = ctxt }
-}

-- Enter a new context: push onto the stack and switch the start code
-- as appropriate
alexPushContext :: ScannerContext -> Alex ()
alexPushContext ctxt = do
  st <- alexGetUserState
  alexSetUserState $ st { usContext = ctxt : usContext st }
  alexSwitchContext ctxt

-- Set the start code appropriately for the given context
alexSwitchContext :: ScannerContext -> Alex ()
alexSwitchContext StartBlock = alexSetStartCode startBlock
alexSwitchContext (InBlock _) = alexSetStartCode inBlock
alexSwitchContext StartBlockNL = alexSetStartCode inBlock
alexSwitchContext Freeform = alexSetStartCode freeform

alexPeekContext :: Alex ScannerContext
alexPeekContext = do st <- alexGetUserState
                     case usContext st of
                       c:_ -> return c
                       []  -> alexError "internal error: peek at empty state"

alexPopContext :: Alex ()
alexPopContext = do
  st <- alexGetUserState
  case usContext st of
   _ : cs -> alexSetUserState $ st { usContext = cs }
   _      -> alexError "internal error: popped at empty state"

nextLine, beginBlock, firstBlockToken ::
  AlexInput -> Int -> Alex Token

-- At the start of the line: check the current context, switching from
-- StartBlockNL to StartBlock if necessary, and continue scanning
nextLine _ _ = do ctxt <- alexPeekContext
                  case ctxt of
                    InBlock _ -> alexSetStartCode startLine
                    StartBlock -> return ()
                    StartBlockNL -> do alexPopContext
                                       alexPushContext StartBlock
                    Freeform -> alexError "internal error: nextLine in Freeform?"
                  alexMonadScan

beginBlock alexInput _ = do alexPushContext (InBlock $ inputCol alexInput)
                            return $ Token (inputPos alexInput) TLbrace

-- At the first token in a block, remove the current state,
-- enter a new block context based on this token, and return a TBegin token
firstBlockToken alexInput l = do alexPopContext -- should be in StartBlock
                                 beginBlock alexInput l


-- At the first token in a line in a block, check the offside rule
firstLineToken :: AlexInput -> Int -> Alex Token
firstLineToken (_,_,_,"") _ = do alexSetStartCode inBlock -- EOF case
                                 alexMonadScan
firstLineToken alexInput _ = do
  ctxt <- alexPeekContext
  let tCol = inputCol alexInput
  case ctxt of
    InBlock col | tCol > col  -> do alexSetStartCode inBlock -- Continued line
                                    alexMonadScan
                | tCol == col -> do alexSetStartCode inBlock -- Next line starts
                                    return $
                                      Token (inputPos alexInput) TSemicolon
                | otherwise   -> do alexPopContext -- but stay in startLine code
                                    return $ Token (inputPos alexInput) TRbrace
                -- FIXME: what about error conditions?
    _ -> alexError "StartBlock or StartBlockNL at first line token?"

data Token = Token AlexPosn TokenType
  deriving (Show, Eq)

data TokenType =
    TEOF
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TLet
  | TCase
  | TOf
  | TEq
  | TPlus
  | TSemicolon
  | TColon
  | TLparen
  | TRparen
  | TLbrace
  | TRbrace
  | TLbracket
  | TRbracket
  | TInteger Integer
  | TString String
  | TId String
  deriving (Show, Eq)

lexerForHappy :: (Token -> Alex a) -> Alex a
lexerForHappy = (alexMonadScan >>=)

-- End any pending blocks
alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{ alex_pos = pos, alex_ust = st } ->
          case dropWhile isntBlock (usContext st) of
            InBlock _: ctxts@(_:_) -> Right (s', Token pos TRbrace)
                where s' = s { alex_ust = st { usContext = ctxts } }
            _ -> Right (s, Token pos TEOF)
            where isntBlock (InBlock _) = False
                  isntBlock _ = True                  


{-

Ideas: looks like we can get rid of the "else exception" -- it will
disambiguate fine with the ';' provided we put in a precedence for ';'

Should we use a different separator for let blocks?

Need to insert a "then" token before the body of the 'if' block.
Need to insert a "do" token before the body of a 'while' block
"of" before the body of a "case"

Use "|" for the separator for case?

-}


}
