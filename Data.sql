SELECT 
    CC.CustomerId,
    CC.CustomerName,
    N.year,
    A.loanId,
A.unitOfMeasure,
A.cashAndCashEquivalent,
A.shortTermLoanAndAdvances,
A.tradeReceivables,
A.inventory,
A.otherCurrentAsset,
A.nonCurrentAsset,
A.shortTermBorrowing,
A.tradePayable,
A.otherCurrentLiabilities,
A.shortTermProvision,
A.longTermBorrowing,
A.otherNonCurrentLiabilities,
A.totalEquity,
    G.netSales,
G.otherIncome,
G.costOfGoodsSold,
G.sellingAndAdministrativeExpense,
G.depreciationAndAmortization,
G.otherExpense,
G.interestExpence,
G.tax,
    (A.totalEquity / (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision + A.longTermBorrowing + A.otherNonCurrentLiabilities)) AS EL,
    (G.netSales - G.costOfGoodsSold) AS NLCOGS,
    (G.interestExpence / (G.sellingAndAdministrativeExpense + G.depreciationAndAmortization + G.otherExpense)) AS IOEXP,
    (G.interestExpence / (G.sellingAndAdministrativeExpense + G.depreciationAndAmortization + G.otherExpense + G.costOfGoodsSold)) AS IOEXP2,
    (A.tradePayable - A.inventory) AS PAYINV2,
    (A.tradePayable / A.inventory) AS PAYINV,
    (A.tradeReceivables - A.tradePayable) AS RCPAY,
    (A.tradeReceivables / A.tradePayable) AS RCPAY2,
    (G.netSales / A.totalEquity) AS NSE,
    (YEAR(CURDATE()) - O.YearOfIncorporation) AS Age,
    O.CompnyType AS Type,
    (A.inventory / G.costOfGoodsSold) * 365 AS IHP,
    (A.tradeReceivables / G.netSales) * 365 AS DSO,
    (A.tradePayable / G.costOfGoodsSold) * 365 AS DPO,
    ((A.inventory / G.costOfGoodsSold) + (A.tradeReceivables / G.netSales) - (A.tradePayable / G.costOfGoodsSold)) * 365 AS CCC,
    ((A.inventory / G.costOfGoodsSold) + (A.tradeReceivables / G.netSales) + (A.tradePayable / G.costOfGoodsSold)) * 365 AS CCC2,
    (G.netSales) / (A.cashAndCashEquivalent + A.shortTermLoanAndAdvances + A.tradeReceivables + A.inventory + A.otherCurrentAsset + A.nonCurrentAsset) AS ATR,
    (G.netSales - G.costOfGoodsSold) / (G.netSales) AS GMP,
    (A.longTermBorrowing + A.shortTermBorrowing) / (A.totalEquity) AS DE,
    (G.interestExpence) / (G.netSales + G.otherIncome - (G.costOfGoodsSold + G.depreciationAndAmortization + G.sellingAndAdministrativeExpense + G.otherExpense)) AS ICR,
    (A.cashAndCashEquivalent + A.shortTermLoanAndAdvances + A.tradeReceivables) / (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision) AS QR,
    (G.netSales + G.otherIncome - (G.costOfGoodsSold + G.sellingAndAdministrativeExpense + G.depreciationAndAmortization + G.otherExpense + G.interestExpence + G.tax)) / (A.cashAndCashEquivalent + A.shortTermLoanAndAdvances + A.tradeReceivables + A.inventory + A.otherCurrentAsset + A.nonCurrentAsset) AS ROA,
    (A.cashAndCashEquivalent) / (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision) AS CashR,
    (A.cashAndCashEquivalent + A.shortTermLoanAndAdvances + A.tradeReceivables + A.inventory + A.otherCurrentAsset) / (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision) AS CurrentR,
    (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision + A.longTermBorrowing + A.otherNonCurrentLiabilities) / (A.cashAndCashEquivalent + A.shortTermLoanAndAdvances + A.tradeReceivables + A.inventory + A.otherCurrentAsset + A.nonCurrentAsset) AS DebtR,
    ((G.netSales + G.otherIncome) - (G.costOfGoodsSold + G.sellingAndAdministrativeExpense + G.depreciationAndAmortization + G.otherExpense)) / G.netSales AS OPM,
    (G.netSales + G.otherIncome) / A.totalEquity AS ROE,
    (G.netSales + G.otherIncome) - (G.costOfGoodsSold + G.sellingAndAdministrativeExpense + G.otherExpense) AS EBITDA,
    (G.netSales + G.otherIncome) - (G.costOfGoodsSold + G.sellingAndAdministrativeExpense + G.otherExpense + G.depreciationAndAmortization) AS EBIT,
    (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision + A.longTermBorrowing + A.otherNonCurrentLiabilities) / A.totalEquity AS LNW,
    (A.shortTermBorrowing + A.tradePayable + A.shortTermProvision + A.longTermBorrowing) / A.totalEquity AS DNW,
    ((G.netSales + G.otherIncome) - (G.costOfGoodsSold + G.sellingAndAdministrativeExpense + G.otherExpense + G.tax + G.interestExpence) - (0.25 * ((A.inventory / G.costOfGoodsSold) + (A.tradeReceivables / G.netSales) + (A.tradePayable / G.costOfGoodsSold)) * G.netSales)) / (A.shortTermBorrowing + A.tradePayable + A.shortTermProvision + A.longTermBorrowing) AS CDSCR,
    ((G.netSales + G.otherIncome) - (G.costOfGoodsSold + G.sellingAndAdministrativeExpense + G.otherExpense)) / (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision + A.longTermBorrowing + A.otherNonCurrentLiabilities + A.totalEquity + G.tax) AS ROCE,
    (A.shortTermBorrowing + A.tradePayable + A.otherCurrentLiabilities + A.shortTermProvision + A.longTermBorrowing + A.otherNonCurrentLiabilities) / (A.cashAndCashEquivalent + A.shortTermLoanAndAdvances + A.tradeReceivables + A.inventory + A.nonCurrentAsset) AS DA
FROM
    (SELECT 
        loanDetailID, loanRequestId
    FROM
        finansme.loanDetail
    GROUP BY 1) BB
        LEFT JOIN
    (SELECT 
        loanRequestId, CustomerId, CustomerName
    FROM
        finansme.loanRequest
    GROUP BY 1) CC ON CC.loanRequestId = BB.loanRequestId
        LEFT JOIN
    finansme.kycBalanceSheet A ON A.loanId = BB.loanDetailID
        JOIN
    (SELECT 
        loanId AS loan, MAX(year) AS year
    FROM
        finansme.kycBalanceSheet
    GROUP BY loan) M ON A.loanId = M.loan
        LEFT JOIN
    finansme.kycPlStatement G ON A.loanId = G.loanId
        JOIN
    (SELECT 
        loanId AS loan_id, MAX(year) AS year
    FROM
        finansme.kycPlStatement
    GROUP BY loan_id) N ON G.loanId = N.loan_id
        LEFT JOIN
    (SELECT 
        loanId, YearOfIncorporation, CompnyType
    FROM
        finansme.kycDetail) O ON O.loanId = A.loanId
WHERE

    A.kycBalanceSheetId IS NOT NULL
    
GROUP BY 1,2,3
