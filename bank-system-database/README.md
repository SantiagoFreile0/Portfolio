# Bank System Database

A relational database system simulating the core operations of a retail bank. Designed from scratch, covering schema design, data integrity, and analytical reporting.

## What It Models

The system manages five operational areas: client and account management, transaction tracking, financial product management, and investment portfolio analysis. The schema has 17 tables organized into three layers: reference tables for business rules, core entity tables for clients, accounts, and products, and transactional tables for movements, investments, and portfolios.

## Views

The most technically interesting part of the project is the set of 10 views built on top of the schema, escalating in complexity:

- Simple joins for client classification and account overviews
- Subselects to identify above-average accounts and high-net-worth clients
- Group by aggregations for monthly cash flow analysis per account
- CTEs with multi-table joins for financial product performance statistics
- Window functions with ranking to compare clients by total invested amount
- Multiple CTEs combined with window functions for monthly portfolio performance reports, ranked by product type and risk level

## Files

- `database_backup_script.sql` — full schema, views, and data

## Technologies

SQL Server, Python, Power BI

*Santiago Freile · 2025*
