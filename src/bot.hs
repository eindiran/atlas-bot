--} IRC bot written in Haskell

import Data.List
import Network
import System.Exit
import System.IO
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf


--} Net monad: wraps IO, carries bots immutable state.
data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

io :: IO a -> Net a
io = liftIO


--} Set up global variables to store channel, server, port and nick.
server    = "irc.freenode.org"
port      = 6667
channel   = "#tutbot-testing"
nickname  = "atlas-bot"
ident     = "AtlasBot"
gecos     = "AtlasBot v0.1"


--} write: Send a message to the server we're connected to.
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t


--} privmsg: Send a private message.
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (channel ++ " :" ++ s)


--} eval: Dispatch a command.
eval :: String -> Net ()
eval     "!quit"                 = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval     "!names"                = get_names
eval x | "!id" `isPrefixOf` x    = privmsg (drop 4 x)
eval x | "!topic" `isPrefixOf` x = set_topic (drop 7 x)
eval x | "!kick" `isPrefixOf` x  = kick (drop 6 x)
eval     _                       = return () -- ignore anything that doesn't match
                                             -- the above patterns
 

--} listen: Process each line from the server.
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING: " `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)


--} connect: Connect to the server and return the initial bot state.
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s  ..." server >> hFlush stdout)
        (putStrLn "done.")
        a


--} run: We're in the Net monad now, so we've connected successfully
--       Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nickname
    write "USER" (ident ++ " 0 * : " ++ gecos)
    write "JOIN" channel
    asks socket >>= listen


--} set_topic: Set the topic of a (mode +t) channel.
set_topic :: String -> Net ()
set_topic s = write "TOPIC" (channel ++ " :" ++ s)
    

--} get_names: Get the names of those in the channel.
get_names :: Net ()
get_names = write "NAMES" (channel ++ " :")


--} kick: Kick as user off of a channel.
kick :: String -> Net ()
kick n = write "KICK" (channel ++ " :" ++ n)


--} main: Setup actions at start, enter run loop.
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st
