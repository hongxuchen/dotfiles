# vim: set ft=gdb ts=4 sw=4 tw=0 et :

define cls
  shell clear
end
document cls
Clear screen.
end

define ls
  shell ls -AF
end
document ls
list current directory file using shell `ls -AF'
end

define print-char
  if ($arg0 > 0xff)
    print "not a character"
    ""
  else
    if ($arg0 == '\n')
      printf "\\n"
    else
      if ($arg0 == '\t')
        printf "\\t"
      else
        if ($arg0 == '\r')
          printf "\\r"
        else
          if ($arg0 == '\'')
            printf "\\'"
          else
            if (($arg0 < 0x20) || ($arg0 >= 0x7f))
              printf "\\%03o", $arg0
            else
              printf "%c", $arg0
            end
          end
        end
      end
    end
  end
  printf "\n"
end
document print-char
Print a single character in a readable fashion.
end

define frame
  info frame
  info args
  info locals
end
document frame
Print stack frame.
end
