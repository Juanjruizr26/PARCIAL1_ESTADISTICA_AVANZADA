#web scraping
library(readxl)
datos <- read_excel("kia.xlsx")

#exploración de datos
require(table1)
table1(~.,data=datos)

#posibles modelos
mod1=lm(PRECIO~ANTIGUEDAD,data=datos)
summary(mod1)

mod2 = lm(PRECIO~KILOMETRAJE,data=datos)
summary(mod2)

mod3 = lm(PRECIO~.,data=datos)
summary(mod3)

#el mejor modelo es el 2
require(ggplot2)
ggplot(data = datos, aes(x = ANTIGUEDAD, y = PRECIO)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(x = "Antigüedad", y = "Precio (millones)") +
  scale_color_manual(values = c("blue", "red")) + theme_bw()

## Validación
require(caret)
control = trainControl(method = "cv",number=276)
mod_caret = train(PRECIO~ANTIGUEDAD,data=datos,method="lm",trControl=control,metric="MAE")
mod_caret

MAE = 4.24
MAPE = MAE/mean(datos$PRECIO)*100
MAPE