open Utils
open Location

open Asmast
open Asmparser
open Asmlexer

open Ilast
open Il



let lift (ast:section) =
  let ilast = ([], []) in
  ilast
