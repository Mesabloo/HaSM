# Documentation

Some symbols in the intel manual are hard to understand.
Here is what I understood:

| Symbol | Example instruction | Example opcodes | Meaning |
|:------:|---------------------|:---------------:|---------|
| `/r`   | `mov dst=r32 src=r32` | `89 /r` | `mod r/m byte | (#src << 3) | #dst` |
|        | `mov dst=r32 src=m32` | `8B /r` | `(#dst << 4) | 0x04 + #src` |
|        | `mov dst=m32 src=r32` | `89 /r` | `(#dst << 4) | 0x04 + #src` |
| `/0`   | `mov dst=r32 src=imm32` | `C7 /0 id` | `0x05 | #dst` |
|        | `mov dst=m32 src=imm32` | `C7 /0 id` | `0x05 + #dst` |
| `/1`   | `or dst=r8 src=imm8` | `80 /1 ib` | `0xC8 | #dst` |
|        | `or dst=m8 src=imm8` | `80 /1 ib` | `0x0D + #dst` |
| `/2`   | `not src=r32` | `F7 /2` | `0xD0 | #src` |
|        | `not src=m32` | `F7 /2` | `0x15 + #src` |
| `/3`   | `neg src=r32` | `F7 /3` | `0xD8 | #src` |
|        | `neg src=m32` | `F7 /3` | `0x1D + #src` |
| `/4`   | `mul src=r32` | `F7 /4` | `0xE0 | #src` |
|        | `mul src=m32` | `F7 /4` | `0x25 + #src` |
| `/5`   | `imul src=r32` | `F7 /5` | `0xE8 | #src` |
|        | `imul src=m32` | `F7 /5` | `0x2D + #src` |
| `/6`   | `div src=r32` | `F7 /6` | `0xF0 | #src` |
|        | `div src=m32` | `F7 /56` | `0x35 + #src` |
| `/7`   | `cmp v2=r32 v1=imm32` | `81 /7 id` | `0xF8 | #v2` |
|        | `cmp v2=m32 v1=imm32` | `81 /7 id` | `0x3D + #v2` |