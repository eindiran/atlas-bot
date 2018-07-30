{-# Language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module       : Main
Description  : A simple IRC bot written in Haskell
Author       : Elliott Indiran <eindiran@uchicago.edu>
Stability    : experimental
Portability  : POSIX
Version      : v0.1.0
-}

module Main where
import GHC.Generics
import Network
import System.Exit
import System.IO
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Data.List
import Data.Bool
import qualified Data.Yaml as Y


--} Store the config in the YAML file here
yamlConfigFilename :: String
yamlConfigFilename = "../config/config.yaml"


--} Wrap the config up together
data Config =
    Config {
        server  :: String,
        channel :: String,
        portNum :: Int
    } deriving (Show, Generic)

instance Y.FromJSON Config


--} HandleIO monad: wraps IO, carries bots immutable state.
data Bot = Bot { socket :: Handle }
type HandleIO = ReaderT Bot IO

io :: IO a -> HandleIO a
io = liftIO


--} Set up global variables to store channel, server, port and nick.
server_t    = "irc.freenode.org"
port_t      = 6667
channel_t   = "#tutbot-testing"
nickname    = "atlas-bot"
ident       = "AtlasBot"
gecos       = "AtlasBot v0.1"


--} write: Send a message to the server we're connected to.
write :: String -> String -> HandleIO ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t


--} privmsg: Send a private message.
privmsg :: String -> String -> HandleIO ()
privmsg channel_t msg = write "PRIVMSG" (channel_t ++ " :" ++ msg)


--} eval: Dispatch a command.
eval :: String -> HandleIO ()
eval     "!quit"                 = write "QUIT:" "Caught !quit command" >> io (exitWith ExitSuccess)
--eval     "!names"                = get_users channel_t
eval     "!list"                 = show_commands
eval     "!help"                 = show_commands
eval x | "!id" `isPrefixOf` x    = privmsg channel_t (drop 4 x)
eval x | "!topic" `isPrefixOf` x = set_topic channel_t (drop 7 x)
eval x | "!kick" `isPrefixOf` x  = kick channel_t (drop 6 x)
eval     _                       = return () -- ignore anything that doesn't match
                                             -- the above patterns
 

--}
show_commands :: HandleIO ()
show_commands = do
    write "PRIVMSG" (channel_t ++ " :" ++ "These are the commands available:")
    write "PRIVMSG" (channel_t ++ " :" ++ "    !help:     Show help message")
    write "PRIVMSG" (channel_t ++ " :" ++ "    !names:    List the users in the channel")
    write "PRIVMSG" (channel_t ++ " :" ++ "    !list:     List available commands")
    write "PRIVMSG" (channel_t ++ " :" ++ "    !quit:     Quit the bot")
    write "PRIVMSG" (channel_t ++ " :" ++ "    !id:       Send private message")
    write "PRIVMSG" (channel_t ++ " :" ++ "    !topic:    Set channel topic")
    write "PRIVMSG" (channel_t ++ " :" ++ "    !kick:     Kick a user from the channel")


--} listen: Process each line from the server.
listen :: Handle -> HandleIO ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING: " `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)


--} connect: Connect to the server and return the initial bot state.
connect :: String -> Int -> IO Bot
connect server_t port_t = notify $ do
    h <- connectTo server_t (PortNumber (fromIntegral port_t))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s  ..." server_t >> hFlush stdout)
        (putStrLn "done.")
        a


--} run: We're in the HandleIO monad now, so we've connected successfully
--       Join a channel, and start processing commands
run :: String -> HandleIO ()
run channel_t = do
    write "NICK" nickname
    write "USER" (ident ++ " 0 * : " ++ gecos)
    write "JOIN" channel_t
    asks socket >>= listen


--} set_topic: Set the topic of a (mode +t) channel.
set_topic :: String -> String -> HandleIO ()
set_topic channel_t new_topic = write "TOPIC" (channel_t ++ " :" ++ new_topic)
    

--} get_users: Get the names of those in the channel.
get_users :: String -> Handle -> HandleIO ()
get_users channel_t h = do
    write "NAMES" (channel_t ++ " :")
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    write "Names list:    " (clean s)
  where
    clean = drop 1 . dropWhile (/= ':') . drop 1


--} kick: Kick as user off of a channel.
kick :: String -> String -> HandleIO ()
kick channel_t user = write "KICK" (channel_t ++ " :" ++ user)


--} main: Setup actions at start, enter run loop.
main :: IO ()
main = do
    let connect_p = connect channel_t port_t
    bracket connect_p disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT (run channel_t) st
