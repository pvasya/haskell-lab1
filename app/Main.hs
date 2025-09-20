{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main = scotty 3000 $
  get "/:word" $ do
    beam <- pathParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]