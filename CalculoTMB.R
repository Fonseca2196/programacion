calculo_tmb <- function(edad, peso, altura, genero, actividad_fisica, objetivo) {
  
  #Autor:Felipe Fonseca López
  #Descripcion: La función calcula la Tasa Metabólica Basal (TMB) de una persona en función de su edad, peso, altura, género y nivel de actividad física, y ajusta la TMB 
  #             según el objetivo de la persona (mantener, bajar o subir de peso). 
  #             También distribuye los macronutrientes (proteínas, grasas y carbohidratos) necesarios según la TMB ajustada.
  
  #Argumentos
  #edad: Edad de la persona en años (número entero).
  #peso: Peso de la persona en kilogramos (número real).
  #altura: Altura de la persona en centímetros (número entero).
  #genero: Género de la persona ("masculino" o "femenino").
  #actividad_fisica: Nivel de actividad física de la persona ("sedentario", "ligero", "moderado", "intenso" o "muy intenso").
  #objetivo: Objetivo de la persona ("mantener", "bajar" o "subir").
  
  ##CODIGO
  
  # Calculamos el TMB usando la fórmula de Mifflin-St Jeor
  if (genero == "masculino") {
    tmb <- 10 * peso + 6.25 * altura - 5 * edad + 5
  } else {
    tmb <- 10 * peso + 6.25 * altura - 5 * edad - 161
  }
  
  # TMB por el nivel de actividad física
  niveles_af <- c("sedentario", "ligero", "moderado", "intenso", "muy intenso")
  af_multiplicadores <- c(1.2, 1.375, 1.55, 1.725, 1.9)
  
  if (!(actividad_fisica %in% niveles_af)) {
    stop("El nivel de actividad física debe ser uno de: sedentario, ligero, moderado, intenso, muy intenso.")
  }
  
  multiplicador <- af_multiplicadores[niveles_af == actividad_fisica]
  tmb_ajustado <- tmb * multiplicador
  
  # calorías necesarias para alcanzar el objetivo
  if (objetivo == "mantener") {
    calorias_necesarias <- tmb_ajustado
  } else if (objetivo == "bajar") {
    calorias_necesarias <- tmb_ajustado - 500
  } else if (objetivo == "subir") {
    calorias_necesarias <- tmb_ajustado + 500
  } else {
    stop("El objetivo debe ser uno de: mantener, bajar, subir.")
  }
  
  # Calculo de macronutrientes
  proteinas <- peso
  grasas <- calorias_necesarias * 0.25 / 9
  carbohidratos <- (calorias_necesarias - (proteinas * 4 + grasas * 9)) / 4
  
  # Imprimimos los resultados
  cat("Tu TMB es de", round(tmb), "calorías.\n")
  cat("Necesitas consumir", round(calorias_necesarias), "calorías para", objetivo, ".\n")
  cat("Deberías consumir", round(proteinas), "gramos de proteínas,",
      round(grasas), "gramos de grasas, y", round(carbohidratos), "gramos de carbohidratos por día.\n")
}
