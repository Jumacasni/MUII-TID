# Cargo el archivo .xls con los datos de accidentes
library(readxl)
prestamo <- read_excel("~/Documentos/master_ingenieria_informatica/TID/practicas/practica4_reglas/prestamo.xlsx")

# Matriz de correlación
library(corrplot)
corrplot(cor(prestamo[c(-10,-11,-12,-13)]), method="number", is.corr=FALSE)

# Elimino el atributo Experience por su fuerte relación con Age
prestamo = prestamo[c(-2,-4)]

# Discretización de Age
table(discretize(prestamo$Age, method="frequency", breaks = 4))
prestamo <- discretizeDF(prestamo, methods = list(
  Age = list(method = "frequency", breaks = 4, 
                       labels = c("23-34", "35-44", "45-54", "55-67"))
        ),
        default = list(method = "none")
  )

# Discretización de CCAvg
table(discretize(prestamo$CCAvg, method="interval", breaks = 5))
prestamo <- discretizeDF(prestamo, methods = list(
  CCAvg = list(method = "interval", breaks = 5, 
             labels = c("[0-2)", "[2-4)", "[4-6)", "[6-8)", "[8-10]"))
        ),
        default = list(method = "none")
)

# Discretización de Income
table(discretize(prestamo$Income, method="frequency", breaks = 5))
prestamo <- discretizeDF(prestamo, methods = list(
  Income = list(method = "frequency", breaks = 5, 
               labels = c("[8-33)", "[33-52)", "[52-78)", "[78-113)", "[113-224]"))
          ),
          default = list(method = "none")
)

# Discretización de la variable Mortgage
hist(prestamo$Mortgage)
prestamo$Mortgage <- cut(prestamo$Mortgage,                  
                            breaks = c(-Inf, 1, 250, +Inf), 
                            labels = c("No tiene hipoteca", "Hipoteca normal",
                            "Hipoteca alta"), right = FALSE)

# Pasar Education a nominal
prestamo$Education <- cut(prestamo$Education,                  
                           breaks = c(1, 2, 3, 4, +Inf), 
                           labels = c("Education level", "Undergrad",
                                      "Graduate", "Advanced"), 
                           right = FALSE)

# Pasar Personal Loan a nominal
prestamo$`Personal Loan` <- cut(prestamo$`Personal Loan`,                  
                         breaks = c(0, 1, +Inf), 
                         labels = c("No acepto prestamo personal"
                                    , "Acepto prestamo personal"), right = FALSE)

# Pasar Securities account a nominal
prestamo$`Securities Account` <- cut(prestamo$`Securities Account`,                  
                                breaks = c(0, 1, +Inf), 
                                labels = c("No tiene cuentas de seguridad",
                                "Tiene cuentas de seguridad"), right = FALSE)

# Pasar CD account a nominal
prestamo$`CD Account` <- cut(prestamo$`CD Account`,                  
                                     breaks = c(0, 1, +Inf), 
                                     labels = c("No tiene cuenta CD", "Tiene cuenta CD"), 
                                     right = FALSE)

# Pasar Online a nominal
prestamo$Online <- cut(prestamo$Online,                  
                             breaks = c(0, 1, +Inf), 
                             labels = c("No usa online", "Usa online"), 
                             right = FALSE)

# Pasar CreditCard a nominal
prestamo$CreditCard <- cut(prestamo$CreditCard,                  
                       breaks = c(0, 1, +Inf), 
                       labels = c("No usa tarjeta", "Usa tarjeta"), 
                       right = FALSE)

# Paso a factor Family
prestamo$Family <- as.factor(prestamo$Family)

library(arules)
prestamo_trans = as(prestamo, "transactions")
summary(prestamo_trans)
itemFrequencyPlot(prestamo_trans, topN = 11)

# Training Apriori on the dataset
rules = apriori(data = prestamo_trans, parameter = list(confidence = 0.95))
inspect(sort(rules, by='count')[1:10])

# Subset RHS = Securities Account
rules.securities_account <- subset(rules, subset = rhs %pin% "Securities Account=")
inspect(sort(rules.securities_account, by='confidence')[1:10])
inspect(sort(rules.securities_account, by='count')[1:10])

# Subset RHS = CD Account
rules.cd_account <- subset(rules, subset = rhs %pin% "CD Account=")
inspect(sort(rules.cd_account, by='confidence')[1:10])
inspect(sort(rules.cd_account, by='count')[1:10])

# Subset LHS = Income y Mortgage
rules.income <- subset(rules, subset = lhs %pin% "Income=" & lhs %pin% "Mortgage=")
inspect(sort(rules.income, by='confidence')[1:10])
inspect(sort(rules.income, by='count')[1:10])

# Subset LHS = Age y Mortgage
rules.education <- subset(rules, subset = lhs %pin% "Age=" & lhs %pin% "Mortgage")
inspect(sort(rules.education, by='confidence')[1:10])
inspect(sort(rules.education, by='count')[1:10])
