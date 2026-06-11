# Nova Language Specification v1.0

## 設計哲學
Nova 是一門強靜態型態、編譯型語言，目標輸出為堆疊機中間碼（Nova Bytecode）。
語法去除大括號、分號等雜訊，以縮排表達區塊。型態推斷減少重複，但所有型態
在編譯期完全確定。無垃圾收集，改採線性所有權（Ownership Lite）。

## 型態系統
- 強型態（Strongly Typed）
- 靜態型態（Statically Typed，編譯期確定）
- 無隱式轉型（所有轉型必須明確）
- 基礎型態：int, float, bool, str, void
- 複合型態：array[T], tuple(T1, T2, ...), fn(T1)->T2

## 目標碼
- Nova Stack Machine（NSM）中間碼
- 可選輸出為 Python 模擬的虛擬機執行

## 記憶體管理
- 無垃圾收集
- 基本值型態（int/float/bool）：Stack 分配
- str/array：引用計數（RC）簡化版，編譯器插入 inc/dec 指令

## EBNF 文法
見 GRAMMAR.ebnf
