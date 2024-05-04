# Función para convertir horas a radianes
RadiansToDecimal <- function(datetime) {
  grados <- horas * 15  # Convertir horas a grados
  radianes <- grados * pi / 180  # Convertir grados a radianes
  return(radianes)  # Devolver el resultado en radianes
}