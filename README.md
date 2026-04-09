#  RPT6000 – Year-To-Date Sales Report V6

## Course Information
**Course:** CIS352 – Intro to Enterprise Computing  
**Assignment:** Chapters 6, 10, & 11  
**Date:** April 9th 2026  

---

## Table of Contents
* [Project Overview](#project-overview)
* [Features](#features)
* [Key Concepts](#key-concepts-implemented)
* [Workflow](#program-workflow)
* [Output](#output)
* [Authors](#authors)
---

## Project Overview

The **RPT6000 program** is a COBOL-based report system built from RPT5000.  
It generates a **Year-To-Date Sales Report** that displays:

- Branch information  
- Sales representative (SALESREP) names  
- Customer data  
- Sales totals (current and previous year)  
- Change amounts and percentages  

This version introduces more advanced COBOL concepts such as:
- `REDEFINES`
- Table processing with indexes
- File handling for dynamic data loading
- COPY libraries for modular design

---

## Features

- Formatted multi-line report output
- Sales comparisons (This YTD vs Last YTD)
- Dynamic SALESREP name lookup from file
- Calculated totals:
  - Salesrep totals
  - Branch totals
  - Grand totals
- Special value handling:
  - `"N/A"` for undefined percentages
  - `"OVRFLW"` for overflow conditions

---

## Key Concepts Implemented

### Chapter 6
- Use of `REDEFINES` for flexible data formatting
- Packed decimal usage at group level
- Handling special values (`N/A`, `OVRFLW`)
- Initialization of totals

### Chapter 10
- Table creation for SALESREP names
- Use of indexing instead of subscripts
- Dynamic lookup of SALESREP names

### Chapter 11
- Use of COPY libraries
- Separation of data structures into reusable components
- JCL updates to include `COBOL.SYSLIB`

---

## Program Workflow

1. Initialize variables and tables  
2. Load SALESREP data from input file  
3. Read customer sales records  
4. Match SALESREP numbers to names  
5. Perform calculations:
   - Sales totals
   - Change amounts
   - Percent changes  
6. Format and print report lines  
7. Output totals at SALESREP, BRANCH, and GRAND levels  

---

## Output
<img width="897" height="786" alt="output" src="https://github.com/user-attachments/assets/024bb611-f592-4582-a51f-1b45a9392394" />

---

## Authors
  
**Hayden Schmidt**

- **GitHub**: [Haschm05](https://github.com/Haschm05)
  
- **Email**: [haschm05@wsc.edu]

**Violet French**

- **GitHub**: [Violet French](https://github.com/Pirategirl9000)
  
- **Email**: [BraedynFrench@gmail.com]

