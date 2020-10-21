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

  <0,normal> {

    @newline { nextLine }
    
    if    { layoutNL TIf }
    else  { layout   TElse }
    while { layoutNL TWhile }
    let   { layout   TLet }
    case  { layoutNL TCase }
    \=    { layout   TEq }

    \(    { \ _ _ -> do alexPushContext StartBlock ; alexMonadScan }
    \)    { endBlock }
  
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


data ScannerContext = InBlock Int  -- In a block with the given left margin
                    | StartBlock   -- Start a block at the next token
                    | StartBlockNL -- Start a block on the next line

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
alexSwitchContext _ = alexSetStartCode normal

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

nextLine, beginBlock, firstBlockToken, firstLineToken, endBlock ::
  AlexInput -> Int -> Alex Token

-- At the start of the line: check the current context, switching from
-- StartBlockNL to StartBlock if necessary, and continue scanning
nextLine _ _ = do ctxt <- alexPeekContext
                  case ctxt of
                    InBlock _ -> alexSetStartCode startLine
                    StartBlock -> return ()
                    StartBlockNL -> do alexPopContext
                                       alexPushContext StartBlock
                  alexMonadScan

beginBlock alexInput _ = do alexPushContext (InBlock $ inputCol alexInput)
                            return $ Token (inputPos alexInput) TBegin

-- At the first token in a block, remove the current state,
-- enter a new block context based on this token, and return a TBegin token
firstBlockToken alexInput l = do alexPopContext -- should be in StartBlock
                                 beginBlock alexInput l


-- At the first token in a line in a block, check the offside rule
firstLineToken (_,_,_,"") _ = do alexSetStartCode normal -- EOF case
                                 alexMonadScan
firstLineToken alexInput l = do
  ctxt <- alexPeekContext
  let tCol = inputCol alexInput
  case ctxt of
    InBlock col | tCol > col  -> do alexSetStartCode normal  -- Continued line
                                    alexMonadScan
                | tCol == col -> do alexSetStartCode normal  -- Next line starts
                                    return $ Token (inputPos alexInput) TSep
                | otherwise   -> endBlock alexInput l        -- Block has ended
                -- FIXME: what about error conditions?
    _ -> alexError "StartBlock or StartBlockNL at first line token?"

endBlock alexInput _ = do alexPopContext
                          return $ Token (inputPos alexInput) TEnd

data Token = Token AlexPosn TokenType
  deriving Show

data TokenType =
    TEOF
  | TIf
  | TElse
  | TWhile
  | TLet
  | TCase
  | TEq
  | TBegin           -- Block begin, implicit
  | TEnd             -- Block end, implicit
  | TSep             -- Block element separator, implicit
  | TInteger Integer
  | TString String
  | TId String
  deriving Show

lexerForHappy :: (Token -> Alex a) -> Alex a
lexerForHappy = (alexMonadScan >>=)

-- End any pending blocks
alexEOF :: Alex Token
alexEOF = Alex $ \s@AlexState{ alex_pos = pos, alex_ust = st } ->
          case dropWhile isntBlock (usContext st) of
            InBlock _: ctxts@(_:_) -> Right (s', Token pos TEnd)
                where s' = s { alex_ust = st { usContext = ctxts } }
            _ -> Right (s, Token pos TEOF)
            where isntBlock (InBlock _) = False
                  isntBlock _ = True                  

{-
atBeginningOfLine :: AlexInput -> Int -> Alex Token
atBeginningOfLine alexInput _ = do
  context <- alexGetContext
  case context of
    [] -> do alexSetStartCode 0  -- Should never happen?
             alexMonadScan
    -- FIXME: other cases

    -- FIXME: drop a context and go to the state that's exposed at the top
    
    -- Idea is that top of user stack always reflects the proper start code
    -- explicit push and pop operations that sets the code appropriately?
-}

}
