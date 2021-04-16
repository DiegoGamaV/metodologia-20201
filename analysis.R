### Passos para análise

## Calcular frequências relativas para plotar histogramas
## Calcular frequências relativas para plotar gráficos de barras entre pares de variáveis
## Calcular correlação de Kendall entre pares de variáveis

library(ggplot2)
library(readxl)
library(stringr)

                        # Modificar caminho absoluto para o seu
responses <- read_excel("Documentos/ufcg/Metodologia/metodologia-20201/data/Características Acadêmicas e Profissionais que Influenciam a Entrada em um Projeto no Curso de Ciência da Computação na UFCG (respostas).xlsx", 
                        col_types = c("skip", "text", "text", 
                                      "text", "text", "text"))
# ----- ATIVIDADES NÃO VINCULADAS ---------------------
unbound_activities_str <- responses$`Selecione abaixo em quais atividades extracurriculares NÃO VINCULADAS ao curso você participou ANTES desse último processo seletivo`
unbound_activities <- rep("", times=length(unbound_activities_str))
unbound_activities_counter <- rep(0, times=length(unbound_activities_str))

counter=0
for (activities in unbound_activities_str) {   
  counter=counter+1
  activities <- gsub("\\s*\\([^\\)]+\\)", "", activities) #remover tudo entre parênteses
  unbound_activities[counter]<-str_split(activities, ", (?=[[:upper:]])")
}

unbound_internship <- rep(FALSE, times=length(unbound_activities))
unbound_work <- rep(FALSE, times=length(unbound_activities))
unbound_projects <- rep(FALSE, times=length(unbound_activities))
unbound_opensource <- rep(FALSE, times=length(unbound_activities))
unbound_mentorship <- rep(FALSE, times=length(unbound_activities))
unbound_courses <- rep(FALSE, times=length(unbound_activities))
unbound_study_group <- rep(FALSE, times=length(unbound_activities))
unbound_competitions <- rep(FALSE, times=length(unbound_activities))

counter=0
for (activities in unbound_activities) {   
  counter=counter+1
  for (activity in activities) {
    if (activity == "Estágio não vinculado à UFCG") {
      unbound_internship[counter] = TRUE;
    }
    if (activity == "Trabalho") {
      unbound_work[counter] = TRUE;
    }
    if (activity == "Projetos pessoais") {
      unbound_projects[counter] = TRUE;
    }
    if (activity == "Contribuições OpenSource") {
      unbound_opensource[counter] = TRUE;
    }
    if (activity == "Eventos de Mentoria") {
      unbound_mentorship[counter] = TRUE;
    }
    if (activity == "Cursos externos") {
      unbound_courses[counter] = TRUE;
    }
    if (activity == "Grupos de estudo") {
      unbound_study_group[counter] = TRUE;
    }
    if (activity == "Competições independentes") {
      unbound_competitions[counter] = TRUE;
    }
    number_activities = length(activities)
    unbound_activities_counter[counter] = number_activities
  }
}

# ----- ATIVIDADES VINCULADAS ---------------------
bound_activities_str <- responses$`Selecione abaixo em quais atividades extracurriculares VINCULADAS ao curso você participou ANTES desse último processo seletivo`
bound_activities <- rep("", times=length(bound_activities_str))
bound_activities_counter <- rep(0, times=length(bound_activities_str))

counter=0
for (activities in bound_activities_str) {   
  counter=counter+1
  activities <- gsub("\\s*\\([^\\)]+\\)", "", activities) #remover tudo entre parênteses
  bound_activities[counter]<-str_split(activities, ", (?=[[:upper:]])")
}

bound_author <- rep(FALSE, times=length(bound_activities))
bound_events_listener <- rep(FALSE, times=length(bound_activities))
bound_events_presenter <- rep(FALSE, times=length(bound_activities))
bound_events_organizer <- rep(FALSE, times=length(bound_activities))
bound_representation <- rep(FALSE, times=length(bound_activities))
bound_extension_ministration <- rep(FALSE, times=length(bound_activities))
bound_extension_colaboration <- rep(FALSE, times=length(bound_activities))
bound_monitor <- rep(FALSE, times=length(bound_activities))
bound_didatic <- rep(FALSE, times=length(bound_activities))
bound_guardians <- rep(FALSE, times=length(bound_activities))

counter = 0
for (activities in bound_activities) {   
  counter = counter + 1
  number_activities = 0
  for (activity in activities) {
    if (activity == "Representação Estudantil") {
      bound_representation[counter] = TRUE
    }
    if (activity == "Autoria ou Co-Autoria de Trabalhos Acadêmicos") {
      bound_author[counter] = TRUE
    }
    if (activity == "Autoria ou Co-Autoria de Trabalhos Acadêmicos") {
      bound_author[counter] = TRUE
    }
    if (activity == "Participação em Eventos como Apresentador") {
      bound_events_presenter[counter] = TRUE
    }
    if (activity == "Participação em Eventos como Ouvinte") {
      bound_events_listener[counter] = TRUE
    }
    if (activity == "Participação em Eventos de Ciência da Computação como Organizador") {
      bound_events_organizer[counter] = TRUE
    }
    if (activity == "Ministração de Atividades de Extensão (e.g. oficinas, minicursos, cursos de extensão)") {
      bound_extension_ministration[counter] = TRUE
    }
    if (activity == "Colaboração de Atividades de Extensão (e.g. oficinas, minicursos, cursos de extensão)") {
      bound_extension_colaboration[counter] = TRUE
    }
    if (activity == "Monitoria em alguma disciplina Específica (obrigatória ou geral)") {
      bound_monitor[counter] = TRUE
    }
    if (activity == "Em Curso ou Aprovado(a) em alguma disciplina de Didática") {
      bound_didatic[counter] = TRUE
    }
    if (activity == "Participação nos Guardians") {
      bound_guardians[counter] = TRUE
    }
    number_activities = length(activities)
    bound_activities_counter[counter] = number_activities
  }
}
# ----- VARIÁVEIS LÓGICAS ---------------------
entered_project_str <- responses$`Você passou no seu último processo seletivo para entrada em projetos?`
entered_project <- rep(FALSE, times=length(entered_project_str))

counter=0
for (response in entered_project_str) {
  counter=counter+1
  if (response == "Sim") {
    entered_project[counter] = TRUE
  }
}

had_referral_str <- responses$`Você foi indicado(a) nesse processo?`
had_referral <- rep(FALSE, times=length(had_referral_str))

counter=0
for (response in had_referral_str) {
  counter=counter+1
  if (response == "Sim") {
    had_referral[counter] = TRUE
  }
}

other_projects_str <- responses$`Antes dessa última seleção, você participou de outros projetos?`
other_projects <- rep(FALSE, times=length(other_projects_str))

counter=0
for (response in other_projects_str) {
  counter=counter+1
  if (response == "Sim") {
    other_projects[counter] = TRUE
  }
}

# ----------------- Construindo Tidy Data -----------------
tidy_dataframe <- data.frame(entered_project,
                            had_referral, 
                            other_projects,
                            unbound_activities_counter,
                            bound_activities_counter,
                            unbound_competitions,
                            unbound_courses,
                            unbound_internship,
                            unbound_mentorship,
                            unbound_opensource,
                            unbound_projects,
                            unbound_study_group,
                            unbound_work,
                            bound_author,
                            bound_didatic,
                            bound_events_listener,
                            bound_events_organizer,
                            bound_events_presenter,
                            bound_extension_colaboration,
                            bound_extension_ministration,
                            bound_guardians,
                            bound_monitor,
                            bound_representation)
# ----------------- Processando Tidy Data -----------------
