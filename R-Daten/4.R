# Allgemein
library(ggplot2)
t <- as.data.frame(Titanic)
adult <- subset(t, Age == 'Adult')
child <- subset(t, Age == 'Child')
died <- subset(t, Survived == 'No')
survived <- subset(t, Survived == 'Yes')
adult_died <- subset(adult, Survived == 'No')
adult_survived <- subset(adult, Survived == 'Yes')
child_died <- subset(child, Survived == 'No')
child_survived <- subset(child, Survived == 'Yes')

# Alter
ggplot(t, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  facet_wrap(~Age) + 
  ggtitle('Alter - Allgemein')
# Gestorben
ggplot(died, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  facet_wrap(~Age) + 
  ggtitle('Alter - Gestorben')
# Überlebt
ggplot(survived, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  facet_wrap(~Age) +
  ggtitle('Alter - Überlebt')
# Überlebt Erwachsene
ggplot(adult_survived, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Alter - Überlebt (Erwachsene)')
# Überlebt Kinder
ggplot(child_survived, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Alter - Überlebt (Kinder)')
# Gestorben Erwachsene
ggplot(adult_died, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Alter - Gestorben (Erwachsene)')
# Gestorben Kinder
ggplot(child_died, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Alter - Gestorben (Kinder)')
# Überprüfen -> Sterberate Kinder 3. Klasse
child_3rd = subset(child, child$Class == '3rd')
ggplot(child_3rd, aes(x = Survived, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Alter - Überlebt/Gestorben (Kinder, 3. Klasse)')
# Überprüfen -> Sterberate Erwachsene
ggplot(adult, aes(x = Survived, y = Freq)) +
  geom_col(width = 0.5) +
  facet_wrap(~Class) +
  ggtitle('Alter - Überlebt/Gestorben (Erwachsene)')

# Geschlecht (Überlebt/Gestorben)
ggplot(t, aes(x = Survived, y = Freq, fill = Sex)) +
  geom_col(width = 0.5) +
  ggtitle('Geschlecht - Allgemein')
# Überlebt
ggplot(survived, aes(x = Sex, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Geschlecht - Überlebt')
# Überlebt Erwachsene
ggplot(adult_survived, aes(x = Class, y = Freq, fill= Age)) +
  geom_col(width = 0.5) +
  facet_wrap(~Sex) +
  ggtitle('Geschlecht - Überlebt (Erwachsene)')
# Überlebt Kinder
ggplot(child_survived, aes(x = Class, y = Freq, fill= Age)) +
  geom_col(width = 0.5) +
  facet_wrap(~Sex) +
  ggtitle('Geschlecht - Überlebt (Kinder)')
# Gestorben Erwachsene
ggplot(adult_died, aes(x = Class, y = Freq, fill= Age)) +
  geom_col(width = 0.5) +
  facet_wrap(~Sex) +
  ggtitle('Geschlecht - Gestorben (Erwachsene)')
# Gestorben Kinder
ggplot(child_died, aes(x = Class, y = Freq, fill= Age)) +
  geom_col(width = 0.5) +
  facet_wrap(~Sex) +
  ggtitle('Geschlecht - Gestorben (Kinder)')
# Tatsächlich haben mehr Männer als Frauen überlebt (in absoluten Zahlen gesehen)
ggplot(survived, aes(x = Sex, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Geschlecht - Überlebt insgesamt')


# Klasse
ggplot(t, aes(x = Class, y = Freq, fill = Age)) +
  geom_col(width = 0.5) +
  facet_wrap(~Survived) +
  ggtitle('Klassen - Allgemein')
# -> Überlebt Erwachsene
ggplot(adult_survived, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Klassen - Überlebt (Erwachsene)')
# -> Überlebt Kinder
ggplot(child_survived, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Klassen - Überlebt (Kinder)')
# -> Gestorben Erwachsene
ggplot(adult_died, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Klassen - Gestorben (Erwachsene)')
# -> Gestorben Kinder
ggplot(child_died, aes(x = Class, y = Freq)) +
  geom_col(width = 0.5) +
  ggtitle('Klassen - Gestorben (Kinder)')
