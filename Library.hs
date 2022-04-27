module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- TP 1 2022.
-- APLICAR DESCUENTO = TOTAL CON DESCUENTO UNITARIO
-- descodiciarProducto,versionBarata

aplicarDescuento :: Number -> Number -> Number
aplicarDescuento precioBase descuento = precioBase - (precioBase * descuento)/100

precioUnitario :: Number -> Number -> Number
precioUnitario aplicarDescuento cantidadUnidades = aplicarDescuento * cantidadUnidades

aplicarCostoDeEnvio :: Number -> Number -> Number
aplicarCostoDeEnvio precioBase envio = precioBase + envio

precioTotal :: Number -> Number -> Number
precioTotal precioUnitario envio = precioUnitario + envio

entregaSencilla :: String -> Bool
entregaSencilla = even.length
-- productoCodiciado: Dado el nombre de un producto, saber si es un producto codiciado.
-- Un producto es codiciado cuando la cantidad de letras en su nombre es mayor a 10.
productoCodiciado :: String -> Bool
productoCodiciado nombreProducto = length nombreProducto > 10  

-- descodiciarProducto
descodiciarProducto :: String -> String
descodiciarProducto nombreProducto = take 10 nombreProducto

-- tendria que hacerlo con composicion pero no me salio xd.
productoDeElite :: String -> Bool
productoDeElite nombreProducto = productoDeLujo nombreProducto && productoCodiciado nombreProducto && not(productoCorriente nombreProducto)

productoCorriente:: String -> Bool
productoCorriente nombreProducto = elem (nombreProducto!!0) ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

productoXL :: String -> String 
productoXL nombreProducto = nombreProducto ++ "XL"

productoDeLujo :: String -> Bool --funcion ELEM
productoDeLujo nombreProducto = elem 'x' nombreProducto || elem 'z' nombreProducto

--versionBarata Dado el nombre de un producto conseguir su versión barata. 
--La misma es el producto descodiciado y con su nombre dado vuelta.
versionBarata :: String -> String
versionBarata nombreProducto = reverse . descodiciarProducto $ nombreProducto



--tipado de algunas funciones.
--take :: Int -> String -> String
--drop :: Int -> String -> String
--head :: String -> Char
--reverse :: String -> String--elem :: Char -> String -> Bool




