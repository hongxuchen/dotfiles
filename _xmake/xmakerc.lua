if is_plat("windows") then
  set_toolchains("clang-cl")
else
  set_toolchains("clang")
end
