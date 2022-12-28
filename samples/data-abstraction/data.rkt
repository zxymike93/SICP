#lang racket

;;; type tagging
;;; 最简单直观的方法，但不灵活（procedure 里需要很多分支）。
;;; 动态类型、类型跟值绑定而不是跟变量绑定、所以 scheme 用类似 type tag 的方式。

;;; data directed
;;; 将分支作为数据（table）表示
;;; 

;;; message passing
;;; procedure as class
