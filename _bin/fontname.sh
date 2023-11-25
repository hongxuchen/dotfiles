#!/usr/bin/env bash
fc-scan -f "Family: %{family}\nFullname: %{fullname}\nStyle: %{style}\nColor: %{color}\n\n" "$@"
