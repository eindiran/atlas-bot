--} IRC bot written in Haskell

import Data.List
import Data.Bool
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
eval     "!names"                = get_users
eval     "!list"                 = show_commands
eval     "!help"                 = show_commands
eval x | "!id" `isPrefixOf` x    = privmsg (drop 4 x)
eval x | "!topic" `isPrefixOf` x = set_topic (drop 7 x)
eval x | "!kick" `isPrefixOf` x  = kick (drop 6 x)
eval     _                       = return () -- ignore anything that doesn't match
                                             -- the above patterns
 
--}
show_commands :: Net ()
show_commands = do
    write "PRIVMSG" (channel ++ " :" ++ "These are the commands available:")
    write "PRIVMSG" (channel ++ " :" ++ "    !help:     Show help message")
    write "PRIVMSG" (channel ++ " :" ++ "    !names:    List the users in the channel")
    write "PRIVMSG" (channel ++ " :" ++ "    !list:     List available commands")
    write "PRIVMSG" (channel ++ " :" ++ "    !quit:     Quit the bot")
    write "PRIVMSG" (channel ++ " :" ++ "    !id:       Send private message")
    write "PRIVMSG" (channel ++ " :" ++ "    !topic:    Set channel topic")
    write "PRIVMSG" (channel ++ " :" ++ "    !kick:     Kick a user from the channel")


--} clean: clean a string from the server.
clean s = drop 1 . dropWhile (/= ':') . drop 1


--} listen: Process each line from the server.
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    let clean_s = (clean s)
    if (names clean_s) then get_users h else eval clean_s
  where
    forever a = a >> forever a
    names x   = "names!" `isPrefixOf` x
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
    

--} get_users: Get the names of those in the channel.
get_users :: Handle -> Net ()
get_users h = do
    write "NAMES" (channel ++ " :")
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    write "Names list:    " (clean s)
  where
    clean = drop 1 . dropWhile (/= ':') . drop 1



--} kick: Kick as user off of a channel.
kick :: String -> Net ()
kick n = write "KICK" (channel ++ " :" ++ n)


--} main: Setup actions at start, enter run loop.
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st
