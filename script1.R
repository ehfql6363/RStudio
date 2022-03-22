lecture <- "빅데이터 처리 수업"
print(lecture)

lecture2 <- print
lecture2("과목명은 빅데이터 처리입니다.")

def <- "선언하는 방법은 '<-'이 기호를 통해 선언"
print(def)

absence <- c(0,0,1,2,1,3) #c(combine)은 벡터를 생성하는 함수
mode(absence) #mode : 데이터 타입 확인하는 함수
#결과 : "numeric"

grade <- factor(c("A", "B", "A+", "D", "C+","D+", "B+","C", "F"), levels = c("A+", "A", "B+", "B", "C+","C", "D+","D", "F"), ordered = "TRUE")
print(grade)
#Factor : 범주형 데이터 사용시. Factor_Name <- factor(x, levels, ordered)

ID <- c(111111,111112,111113,111114,111115,111116)
university <- c("삼육대학교", "삼육대학교", "삼육대학교", "삼육대학교", "삼육대학교", "삼육대학교")
college <- c("인문사회대학", "미래융합대학", "인문사회대학", "미래융합대학", "인문사회대학", "미래융합대학")
gpa <- c(4.3, 3.4, 2.3, 4.5, 3.7, 2.4)
grade <- c("A+", "B", "C", "A+", "B+", "C")

Academy <- data.frame(ID, university, college, absence, gpa, grade, stringsAsFactors = FALSE)

Academy[5, ]
Grade_1 <- Academy[, c("college", "grade")]
Academy[c(2, 4, 6), c("college", "gpa")]


#441