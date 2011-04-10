{-# OPTIONS -cpp #-}

-- Adapted from Bery Rinaldo's "Export Playlist to M3U" script. The original copyright
-- information for this script is as follows:

-- Export Playlist to M3U
--
-- This AppleScript is used to output the current iTunes playlist into an M3U format file suitable for use with the AudioTron.

-- Copyright (C) 2002 Bery Rinaldo 
--
-- This program is free software; you can redistribute it and/or modify it under the terms
-- of the GNU General Public License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version. 
--
-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
-- without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details. 
--
-- You should have received a copy of the GNU General Public License along with this program; if not, 
-- write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 

-- Script written by Bery Rinaldo beryrinaldo@mac.com on 18-March-2002
--

module Applescript where

playlistScript :: String -> FilePath -> String
playlistScript in_name out_name = 
      "#!/usr/bin/osascript\nproperty |linefeed| : ASCII character 10\non run\ntry\n\
   with timeout of 1800 seconds\n\
    tell application \"iTunes\"\n\
     set the playlist_text to \"\"\n\
     set the playlist_name to \"" ++ in_name ++ "\"" ++
     "\nset the target_file to POSIX file \"" ++ out_name  ++ "\" as text" ++
      "\ntell playlist playlist_name\n\
       \nset the track_count to the count of tracks\n\
      \nend tell\n\
      \nrepeat with z from 1 to the track_count\n\
       \n\n\
       \ntell track z of playlist playlist_name\n\
        \ncopy {location} to {loc}\n\
       \nend tell\n\
       \nset the posixloc to POSIX path of loc\
       \nset the playlist_text to the playlist_text & posixloc & |linefeed|\n\
      \nend repeat\n\
        \n                 end tell\n\
    \nset this_result to my write_to_file(playlist_text, target_file)\n\
    \nif this_result is not true then\n\
     \nerror this_result -- \"There was a problem writing the playlist file.\"\n\
    \nend if\n\
   \nend timeout\n\
  \non error error_message number error_number\n\
   \ntell application \"iTunes\"\n\
    \nif the error_number is not -128 then\n\
     \nbeep\n\
     \ndisplay dialog error_message buttons {\"Cancel\"} default button 1\n\
    \nend if\n\
   \nend tell\n\
  \nend try\nend run\n" ++

  "on write_to_file(this_data, target_file)\n\
  try\n\
    set the target_file to the target_file as text\n\
    set the open_target_file to open for access file target_file with write permission\n\
    set eof open_target_file to 0\n\
    write this_data to the open_target_file\n\
    close access the open_target_file\n\
    return true\n\
  on error error_message\n\
    try\n\
      close access file target_file\n\
    end try\n\
    return error_message\n\
  end try\n\
  end write_to_file\n"

  