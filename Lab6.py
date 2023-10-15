import re

#1.	Genere una expresión regular que sea capaz de 
# detectar las placas de un vehículo particular guatemalteco.
expresion_regular = r"^[POC] \d{3}[BCDFGHJKLMNPQRSTVWXYZ]{3}$"

placas = ["P 123BTJ", "O 567MNP", "C 987WRT", "J 345XYZ", "P 234AYZ"]
for placa in placas:
    if re.match(expresion_regular, placa):
        print(f"La placa {placa} es válida.")
    else:
        print(f"La placa {placa} no es válida.")

print("*************************************")
# 2.	Genere una expresión regular que valide si un archivo es de tipo .pdf o jpg.
#a.	Ejemplo1.pdf, prueba2.PDF, respuestas_del_examen.jpg, amor.JPG

expresion_regular = r'^.*\.(pdf|jpg|PDF|JPG)$'
nombres_archivo = ["Ejemplo1.pdf", "prueba2.PDF", "respuestas_del_examen.jpg", "amor.JPG", "archivo.txt"]

for nombre in nombres_archivo:
    if re.match(expresion_regular, nombre):
        print(f"El archivo {nombre} es un archivo .pdf o .jpg válido.")
    else:
        print(f"El archivo {nombre} no es un archivo .pdf o .jpg válido.")

print("*************************************")
#3.	Genere una expresión regular para validar contraseñas de correo. 
# Una contraseña de correo debe contener por lo menos 8 caracteres, 
# una letra mayúscula y un carácter especial.
def validar_contrasena(correo):
    expresion_regular = r'^(?=.*[A-Z])(?=.*[!@#$%^&*()_+|~\-=`{}[\]:";\'<>?,./])\S{8,}$'
    if re.match(expresion_regular, correo):
        return True
    else:
        return False

contrasenas = ["Abc@1234", "abc12345", "AbCdEfG#", "!Pa$$w0rd"]
for contrasena in contrasenas:
    if validar_contrasena(contrasena):
        print(f"La contraseña '{contrasena}' es válida.")
    else:
        print(f"La contraseña '{contrasena}' no es válida.")


print("*************************************")
#4.	Cree una expresión regular para validar un numero de carnet de la Universidad Galileo,
# por ejemplo 19002324 donde los primeros dos dígitos representan el año en el que el alumno se inscribió
#  los cuales pueden variar desde el 01 (año 2001) hasta el 30 (año 2030).
#  Los siguientes dos dígitos son cero (00) los cuales van por default y
#  los últimos cuatro dígitos son un número que va desde el 1110 hasta el 8970. 
# Para dar su respuesta utilice la notación de expresiones regulares.
def validar_numero_carnet(carnet):
    expresion_regular = r'^(0[1-9]|1\d|2\d|30)00(1[1-9][1-9][0-9]|8[0-8][0-9][0-9]|8970)$'
    if re.match(expresion_regular, carnet):
        return True
    else:
        return False

carnets = ["19002324", "30001200", "21000050", "22008970", "21001000", "18001234", "23008090"]

for carnet in carnets:
    if validar_numero_carnet(carnet):
        print(f"El número de carné '{carnet}' es válido.")
    else:
        print(f"El número de carné '{carnet}' no es válido.")


print("*************************************")
#6.	Cree una expresión regular para obtener los números telefónicos de Guatemala.
#  Estos pueden contener al inicio +502 o 502, pueden estar separados por un espacio en blanco o un guión o juntos.
#  Notar que los números telefónicos pueden empezar únicamente con 4,5,6 o 2.
#a.	+50254821151, 4210-7640, 52018150, 2434 6854, 11234569, 50211234578

def validar_numero_telefono(telefono):

    expresion_regular = r'(\+502|502)?[2-6]([- ]?\d){7}'

    if re.match(expresion_regular, telefono):
        return True
    else:
        return False

telefonos = ["+50254821151", "4210-7640", "52018150", "2434 6854", "11234569", "50211234578"]

for telefono in telefonos:
    if validar_numero_telefono(telefono):
        print(f"El número de telefono '{telefono}' es valido.")
    else:
         print(f"El número de telefono '{telefono}' no es valido.")

print("*************************************")
# 7.	Genere una expresión regular que sea capaz de obtener correos de la UFM.
def validar_correo_ufm(correo):
    expresion_regular = r'^[a-zA-Z0-9._%+-]+@ufm.edu$'

    if re.match(expresion_regular, correo):
        return True
    else:
        return False

correos_ufm = ["steph@ufm.edu", "oscarpiastri@ufm.edu", "carlossainz@gmail.com", "landonorris@hotmail.edu","lewishamilton@outlook.edu","danielricciardo@uvg.edu"]

for correo in correos_ufm:
    if validar_correo_ufm(correo):
        print(f"El correo '{correo}' es una dirección de la UFM válida.")
    else:
        print(f"El correo '{correo}' no es una dirección de la UFM válida.")