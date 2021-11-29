
data Article = Text String
             | Section String [Article] deriving (Show)
data Company = {name :: String, }
myArticle :: Article
myArticle = Section "Document" [
    Section "Introduction"[
        Text "MyIntroduction",
        Section "Notation" [Text "alpha beta gamma"]],
    Section "Methods" [
        Section "Functional programming" [Text "FPR"],
        Section "Logical Programming" [Text "LPR"]],
    Section "Results" [Text "All is great"]]

allSectionNames :: Article -> [String]
allSectionNames (Text _) = []
allSectionNames (Section x []) = [x]
allSectionNames (Section x (y:ys)) = allSectionNames (Section x ys) ++ allSectionNames y

data Entity = Point (Double, Double)
            | Circle (Double, Double, Int)
            | Container [Entity] deriving (Show)

myEntity :: Entity
myEntity = Point (2,2)


data Article = Text String
             | Section String [Article] deriving (Show)

myArticle :: Article
myArticle = Section "Document" [
    Section "Introduction"[
        Text "MyIntroduction",
        Section "Notation" [Text "alpha beta gamma"]],
    Section "Methods" [
        Section "Functional programming" [Text "FPR"],
        Section "Logical Programming" [Text "LPR"]],
    Section "Results" [Text "All is great"]]

allTexts :: Article -> [String]    
allTexts (Text x) = [x]
allTexts (Section x [])=[]
allTexts (Section x (a:as)) = allTexts (Section x as) ++ allTexts a

names :: Article -> [String]
names (Text _) = []
names (Section [] _) = []
names (Section x []) = [x]
names (Section x (y:ys)) = names (Section x ys)
