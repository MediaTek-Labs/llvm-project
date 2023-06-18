set(LLVM_LINK_COMPONENTS
  Core
  IRReader
  Support
  )

add_llvm_tool(llvm-lto-sections
  llvm-lto-sections.cpp
  )
