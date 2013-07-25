(ns toothpick.dcpu16
  (:require [toothpick.core :refer :all]))

;; An assembler for Notch's fictional DCPU16 as documented here:
;;     http://dcpu.com/dcpu-16/
;; define architectural registers

(def dopfmt [6 5 5])

(define-architecture dcpu16
  ;; general purpose regist ers...
  ;;------------------------------------------------------------------------
  (reg 0x00 "A")
  (reg 0x01 "B")
  (reg 0x02 "C")
  (reg 0x03 "X")
  (reg 0x04 "Y")
  (reg 0x05 "Z")
  (reg 0x06 "I")
  (reg 0x07 "J")

  ;; special purpose registers...
  ;;------------------------------------------------------------------------
  (reg 0x1b "SB")
  (reg 0x1c "PC")
  (reg 0x1d "EX")

  ;; opcodes...
  ;;------------------------------------------------------------------------
  (op 0x01 "SET" dopfmt
      "sets b to a")

  (op 0x02 "ADD" dopfmt
      "sets b to b + a, sets EX to 0x0001 on overflow")

  (op 0x03 "SUB" dopfmt
      "sets b to b - a, sets EX to 0xFFFF on underflow")

  (op 0x04 "MUL" dopfmt
      "sets b to b*a, sets EX to ((b*a)>>16)&0xffff)."
      " (treats b, a as unsigned)")

  (op 0x05 "MLI" dopfmt
      "like MUL, but treat b, a as signed")

  (op 0x06 "DIV" dopfmt
      "sets b to b/a, sets EX to ((b<<16)/a)&0xffff."
      " if a==0, sets b and EX to 0 instead. (treats b, a as unsigned)")

  (op 0x07 "DVI" dopfmt
      "like DIV, but treat b, a as signed. Rounds towards 0")

  (op 0x08 "MOD" dopfmt
      " sets b to b%a. if a==0, sets b to 0 instead.")

  (op 0x09 "MDI" dopfmt
      "like MOD, but treat b, a as signed. (MDI -7, 16 == -7)")

  (op 0x0a "AND" dopfmt
      "sets b to b&a")

  (op 0x0b "BOR" dopfmt
      "sets b to b|a")

  (op 0x0c "XOR" dopfmt
      "sets b to b^a")

  (op 0x0d "SHR" dopfmt
      "sets b to b>>>a, sets EX to ((b<<16)>>a)&0xffff"
      " (logical shift)")

  (op 0x0e "ASR" dopfmt
      "sets b to b>>a, sets EX to ((b<<16)>>>a)&0xffff"
      " (arithmetic shift) (treats b as signed)")

  (op 0x0f "SHL" dopfmt
      "sets b to b<<a, sets EX to ((b<<a)>>16)&0xffff")

  (op 0x10 "IFB" dopfmt
      "performs next instruction only if (b&a)!=0")

  (op 0x11 "IFC" dopfmt
      "performs next instruction only if (b&a)==0")

  (op 0x12 "IFE" dopfmt
      "performs next instruction only if b==a")

  (op 0x13 "IFN" dopfmt
      "performs next instruction only if b!=a")

  (op 0x14 "IFG" dopfmt
      "performs next instruction only if b>a")

  (op 0x15 "IFA" dopfmt
      "performs next instruction only if b>a (signed)")

  (op 0x16 "IFL" dopfmt
      "performs next instruction only if b<a")

  (op 0x17 "IFU" dopfmt
      "performs next instruction only if b<a (signed)")

  (op 0x1a "ADX" dopfmt
      "sets b to b+a+EX, sets EX to 0x0001 if there is an overflow,"
      " 0x0 otherwise.")

  (op 0x1b "SBX" dopfmt
      "sets b to b-a+EX, sets EX to 0xFFFF if there is an under-flow,"
      " 0x0001 if there's an overflow, 0x0 otherwise")

  (op 0x1e "STI" dopfmt
      "sets b to a, then increases I and J by 1")

  (op 0x1f "STD" dopfmt
      "sets b to a, then decreases I and J by 1"))
