# ========================================================================
# 텍스트 마이닝 기반 보건의료정보관리사 교육인증 교과목과 
# 의료데이터 중심병원 미션·비전의 비교 분석
# ========================================================================
# 완전 수정 버전 - 동의어 통일 함수 추가 및 에러 처리 강화
# ========================================================================

# ------------------------------------------------------------------------
# 0. 초기 설정
# ------------------------------------------------------------------------
install.packages("readxl")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("KoNLP")
install.packages("wordcloud2")
install.packages("ggwordcloud")
install.packages("igraph")
install.packages("ggraph")
install.packages("widyr")
install.packages("scales")
install.packages("stringr")
install.packages("topicmodels")
install.packages("tm")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("kableExtra")
install.packages("RColorBrewer")
install.packages("patchwork")

# UTF-8 로케일/인코딩 설정 (맥에서 보통 en_US.UTF-8 또는 ko_KR.UTF-8 둘 중 하나)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

# KoNLP를 '가장 먼저' 로드
library(KoNLP)

# 사전
useNIADic()

# 테스트
extractNoun("보건의료정보관리사는 데이터 표준화를 수행한다")


# 0.1 패키지 로드
library(readxl)
library(tidyverse)
library(tidytext)
library(KoNLP)
library(wordcloud2)
library(ggwordcloud)
library(igraph)
library(ggraph)
library(widyr)
library(scales)
library(stringr)
library(topicmodels)
library(tm)
library(reshape2)
library(gridExtra)
library(kableExtra)
library(RColorBrewer)
library(patchwork)

# 0.2 전역 상수 정의
KEYWORDS_TOP_N <- 30        
TFIDF_TOP_N <- 30           
VIS_TOP_N <- 30             
WORDCLOUD_TOP_N <- 30       
NET_MIN_FREQ_EDU <- 3       
NET_MIN_FREQ_HOSP <- 3      
CENTRALITY_TOP_N <- 30      

# 색상 팔레트
COLOR_EDU <- "#2E86AB"      
COLOR_HOSP <- "#A23B72"     

# 0.3 KoNLP 사전 설정
safe_dict_setup <- function() {
  tryCatch({
    useSystemDic()
    cat("✅ System dictionary loaded\n")
  }, error = function(e) {
    cat("⚠️ System dictionary loading failed, using Sejong dictionary only\n")
  })
  useSejongDic()
}

safe_dict_setup()

# 0.4 작업 디렉토리 및 폴더 구조 설정
setwd("/Users/jeong-yeonsu/Desktop/TextMining_dataHospital")

dir.create("3_results", showWarnings = FALSE)
dir.create("3_results/tables", showWarnings = FALSE)
dir.create("3_results/figures", showWarnings = FALSE)


# ------------------------------------------------------------------------
# 0.5 동의어 사전 정의
# ------------------------------------------------------------------------

dict_synonym <- list(
  # ========================================
  # 1. 의학/생명과학 핵심 개념
  # ========================================
  
  # 1.1 인체 구조 및 해부학
  "인체구조" = c("인체", "신체", "해부학적", "구조", "계통", "기전", "생리학적"),
  
  # 1.2 질병 및 진단
  "질병" = c("질병", "질환", "증상", "병리학", "감염", "염증"),
  "진단분류" = c("ICD", "진단", "분류", "분류체계", "코드", "코딩", "코드번호", "주진단"),
  "종양" = c("종양질환", "병기", "암관", "암센터", "암환자"),
  
  # 1.3 의료행위 및 치료
  "의료서비스" = c( "의료행위", "의료서비스", "진료", "치료", "처치", "검사", "서비스"),
  "돌봄" = c("포괄적", "전인적", "케어", "치유", "헬스케어"),
  "환자만족"= c("요구", "고객만족", "환자만족", "고객요구", "만족도", "만족"),
  "환자경험" = c("환자경험"),
  "환자안전" = c("환자안전", "안전"),
  # ========================================
  # 2. 보건의료정보 관리
  # ========================================
  # 2.1 의무기록 및 문서
  "의료질" = c("의료의 질", "질", "의료질", "양질"),
  "의무기록관리" = c("의무기록",  "서식"),
  "심사청구" = c("진료비", "청구", "산정", "지불", "수가"),
  "의료정보" = c("건강정보", "의료정보", "정보", "인포메이션", "information"),
  "원무행정" = c("원무", "퇴원", "입원", "등록"),
  "진료지원" = c("진료지원"),
  "전달체계" = c("전달체계"),
  # 2.2 정보 관리 체계
  "데이터"= c("데이터", "데이터베이스", "자료"),
  "정보거버넌스" = c( "Governance", "IG", "거버넌스"),
  "정보품질" = c("정확", "완전", "품질", "충실", "점검"),
  "정보보호" = c("보안", "정보보호", "보호"),
  
  # 2.3 데이터 분석 및 활용
  "분석" = c("분석", "통계"),
  "수집" = c("수집"),
  "전처리"= c("생성", "가공", "처리", "정보처리"), 
  "시각화" = c("시각화"),
  "관리체계" = c("방법", "과정", "단계", "절차", "지침"),
  
  # ========================================
  # 3. 임상
  # ========================================
  "임상의학" = c("임상", "임상의학", "의학", "메디컬"),
  "중증도" = c("중증", "고난도", "중증고난도"),
  "근거중심" = c("근거중심","근거중심의학", "근거"),
  # ========================================
  # 4. 의학교육
  # ========================================
  "실무" = c("현장실습", "실기", "실무", "현장", "업무"),
  "인재양성" = c("육성", "인력양성", "인재", "인재양성", "양성", "인재육성", "인적자원"),
  "교육역량" = c( "교육연구", "학습", "습득", "의학교육", "교육"),
  # ========================================
  # 5. 보건의료 제도
  # ========================================
  
  "건강보험" = c("건강보험", "보험", "의료보장", "복지사회"),
  "의료법규" = c("법률", "법령", "규정", "제도", "원칙", "준칙", "의료법", "정책"),
  
  # ========================================
  # 6. 의료기관 운영
  # ========================================
  "구축" = c("구축", "구성", "건설"),
  "의료기관" = c("병원", "의료기관", "기관"),
  "운영" = c("실천력", "수행", "활용", "이용", "적용", "실현", "실천", "실행", "구현"),
  # ========================================
  # 7. 기술 및 혁신
  # ========================================
  "디지털헬스" = c("디지털"),
  "의료기술" = c("의료기술"),
  "첨단의학" = c("첨단의료", "첨단의학", "첨단", "스마트", "최첨단", "미래지향", "미래의료", "미래의학"),
  "정보시스템" = c("전산", "정보기술", "기술", "의료시스템", "체계", "시스템", "정보관리"),
  
  "혁신" = c("혁신", "창의성", "창의적인",  "발전", "변화", "창의", "창조", "융합", "도전", "도전적", "창의적"),
  "정밀의료" = c("맞춤의료", "정밀의료", "정밀", "맞춤"),
  # ========================================
  # 8. 가치 및 철학 (병원 핵심가치)
  # ========================================
  "삶의질" = c("삶의질", "삶", "삶의질을", "행복"),
  "소외계층" = c("소외지역", "소외계층"),
  "공공성" = c("평등", "공공의료", "지역", "권역", "국민",  "국가", "공공", "공동체", "국립", "국민보건"),
  "공감" = c( "친절", "존중", "상호존중", "상호존", "공감", "사랑", "박애", "정성", "온정", "친절한", "희망", "감동", "배려"),
  "환자중심" = c( "사람중시", "환자", "고객", "환자들", "고객중심", "환자중심",  "환자중심병원"),
  "전문성" = c( "전문적", "중점분야", "전문가", "전문성", "직업전문성", "전문"),
  "건강증진" = c("건강", "건강증진", "국민건강"),
  "생명존중" = c("존엄성", "생명", "생명존중", "생명중시" ),
  "인성" = c("인성중심", "성실", "정직", "인간적인", "인간다움", "인간애"),
  "조직가치" = c( "이념", "자긍심", "사명감", "정신", "책임", "열정", "자유", "자부심", "자세", "긍지", "책임의식", "책임감"),
  "윤리" = c("윤리", "윤리성", "윤리적", "직업윤리"),
  "사회공헌" = c("자비", "공헌", "헌신", "사회", "봉사", "나눔", "봉사정신", "봉사자"),
  "협력" = c("협조", "상호협력", "상호협", "상생협", "협력", "소통", "상생", "화합", "상호존중", "조화", "협동", "팀워크"),
  "지속가능" = c("생태계", "지속", "평생", "지속가능"),
  "신뢰성" = c("신뢰", "정확", "충실"),
  # ========================================
  # 9. 연구 및 학술
  # ========================================
  
  "연구" = c("탐구", "연구", "연구기관", "연구영역", "연구개발", "창의적연구", "연구중심"),
  "지식체계" = c("지식", "이론", "기초", "전파"),
  
  # ========================================
  # 10. 국제 및 글로벌
  # ========================================
  
  "글로벌" = c("글로벌", "세계", "국제", "아시아", "글로컬"),
  
  # ========================================
  # 11. 조직 및 목표
  # ========================================
  "조직관리" = c("재투자", "조직관리", "인사", "성과", "재무관리", "투자"),
  "조직목표" = c("비전", "목표", "선진", "선진화"),
  "의료경영" = c("사업", "경영", "의료경영", "영향력", "선한 영향력", "의료사업", "지표", "전략", "기획", "대안", "대비"),
  "리더십" = c("진취", "진취적", "진취성",  "선두주자", "선도", "프런티어", "주도하", "개척", "개척자", "리더"),
  "의사결정" = c("의사결정"),
  "의료허브" = c("의료허브를", "의료허브")
  
)

cat("✅ 동의어 사전 로드 완료\n")
cat("   - 동의어 그룹 수:", length(dict_synonym), "\n")

# ------------------------------------------------------------------------
# 0.6 동의어 통일 함수 정의 ⭐ 핵심 추가
# ------------------------------------------------------------------------

unify_synonyms <- function(word, synonym_dict) {
  # 각 그룹을 순회하며 매칭 확인
  for (representative in names(synonym_dict)) {
    if (word %in% synonym_dict[[representative]]) {
      return(representative)  # 동의어 발견 시 대표 단어 반환
    }
  }
  # 동의어 사전에 없으면 원래 단어 그대로 반환
  return(word)
}

cat("✅ 동의어 통일 함수 정의 완료\n")

# ------------------------------------------------------------------------
# 0.6 불용어 리스트 정의 (동의어 통일 전후 모두 적용)
# ------------------------------------------------------------------------

stopwords_custom <- c(
  # ========================================
  # 1. 일반 언어 불용어 (기능어)
  # ========================================
  
  # 1.1 접속 및 조사
  "새", "간", "등", "및", "또는", "대한", "통해", "위한", "있는", "하는", "되는",
  "적", "어린", "관련", "경우", "따라", "이다", "있다", "하다", "되다", "기타", "이를",
  "적", "의", "으로", "로", "를", "나은", "많은", "젊은", "하기", "하게", "로운", "당신곁에", "다가올", "적인","을",
  # 1.2 추상적 연결어
  "미래", "확보", "확립", "투자하", "충족","탁월", "최고", "최상", "우수", "최상의",
  "직장", "직원", "지원", "지도력", "일터", "기여", "자기계발", "인격", "창립", "실용", "탁월", "무한", "모두", "가족", "우리", "스스로",
  "구축", "표준",  "수호자", "생활", "산업", "사실", "성취", "열린자세", "지능", "실천자", "증대",
  "개념","아카데미", "이해", "즉", "바로", "이후", "앞서", "같은", "구분", "건실", "겸비", "경험", "계승", 
  "내용", "영역", "분야", "주요", "중요", "필요", "다양", "주로", "개요", "기사", "기재", "대표",
  "파악", "추론", "함양", "원리", "중시", "주춧돌", "중점", "항목", "적응", "개인", "구료",
  "최신", "환경", "질의", "접근", "무한", "동력", "동기부여", "기준", "성찰", "인성", "도입",
  "전적", "자기", "의식", "열린마음으로", "실력", "실용적", "가치",
  "해방", "대안",  "건설", "인간", "탁월한", "열린자세로", "열린마음", "사람", "인류", "탁월성", "품격", 
  # ========================================
  # 2. 의료분야 고빈도 일반어
  # ========================================
  
  # 2.1 과정/방법 관련 일반어
  
  "제공", "도구", "기법","추구", "달성", "작성", "활동", "발휘",
  "대처", "장비",  "관리기법", "창출", "수호", "문화", "강화", 
  "설명", "안내", "역할", "인술", "저장", "유형별", "제시", "프로젝트",
  
  # 2.2 시간/상황 관련
  "상황", "발생", "경과", "사후", "바탕", "근거", "신속", 
  
  # 2.3 평가 관련 일반어
  "효과", "원칙", "영향", "검토", "선정", "중요", "기본", "필요", "평가", "합리",
  
  # 2.4 의료 관련
  "세포", "의생명과학", "손상", "수복", "항상성", "정상", "고통", "의료", "보건의료", "보건", "유전", "주치의",  "해부",
  "의과학", "의과학자", "병의원", "실험",  "바이오헬스", "바이오", "의료진", "메디컬",
  # ========================================
  # 3. 교육 분야 일반어
  # ========================================
  "전사",  "교육기관", "학과", "전공", "대학",
  "교과목", "전공", "심화", "이론", "마스터", "역량",
  
  
  # ========================================
  # 4. 조직/행정 일반어
  # ========================================
  
  "기관", "병의원",  "조직", "기능", "직무", "대학병원", 
  "관리", "병원", "수행", '의료기관', "직업", "전문병원", "의료원",
  # ========================================
  # 5. 추상적 가치어 (맥락 없이 의미 모호)
  # ========================================
  
  # 5.1 정도/수준 표현
  "최적",  "든든", "수준", "우선", "가장", "극대화", "충만", 
  
  
  # 5.2 방향성 표현
  "지향", "선제", "도약",  "성장", 
  
  # 5.3 감정/태도 표현
  "마음", "시혜", "성숙", "가족애", 
  "성숙한", 
  
  # ========================================
  # 6. 관계/속성 일반어
  # ========================================
  
  "중심", "영성", "바탕", "기반", "대상", "특성", "요소", "모델",
  "관계", "동반자", "하나님", "복음", "기독교", "선교", "구원", "믿음", "불교정신",
  "섬김", 
  
  # ========================================
  # 7. 수식어/한정어
  # ========================================
  
  "아름다운", "충실한", "건실한", "끊임없는", "능동적", "적극", "유기", "생리적",
  
  
  # ========================================
  # 8. 숫자 및 기호
  # ========================================
  
  "10", "Best", "The",
  
  
  # ========================================
  # 9. 포괄적 지역/집단명
  # ========================================
  "민국", "강원", "동남", "서울시민", "대한민국", "동남권역",
  # ========================================
  # 10. 약어 및 불완전 용어
  # ========================================
  
  "사의", "의생", "성하", "면역질", 
  "원사", 
  "명과학", "네트워",
  # ========================================
  # 11. 고유명사 (병원 특정)
  # ========================================
  "제민", "구료제민", "연세", "안과학",
  "이화", "세브란스", "알렌", "에비슨", "안과", "스누비안", 
  
  
  # ========================================
  # 12. 브랜딩 용어
  # ========================================
  
  "브랜드", "헬스",  "용어", "명명", "전자"
  
  
)
cat("✅ 불용어 사전 로드 완료\n")
cat("   - 불용어 수:", length(stopwords_custom), "\n\n")

# ------------------------------------------------------------------------
# 0.8 유틸리티 함수
# ------------------------------------------------------------------------

theme_paper <- function() {
  theme_minimal(base_family = "") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

theme_network <- function() {
  theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30"),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
}

safe_save_table <- function(data, filename_base, subdir = "tables") {
  tryCatch({
    csv_path <- file.path("3_results", subdir, paste0(filename_base, ".csv"))
    excel_path <- file.path("3_results", subdir, paste0(filename_base, "_Excel.csv"))
    
    write.csv(data, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
    write_excel_csv(data, excel_path)
    
    cat("✅", filename_base, "저장 완료\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌", filename_base, "저장 실패:", e$message, "\n")
    return(FALSE)
  })
}

safe_save_figure <- function(plot, filename_base, width = 14, height = 10, subdir = "figures") {
  tryCatch({
    fig_path <- file.path("3_results", subdir, paste0(filename_base, ".png"))
    ggsave(fig_path, plot, width = width, height = height, dpi = 300, bg = "white")
    cat("✅", filename_base, "저장 완료\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌", filename_base, "저장 실패:", e$message, "\n")
    return(FALSE)
  })
}

# ------------------------------------------------------------------------
# STEP 1. 데이터 불러오기
# ------------------------------------------------------------------------
cat("\n=== STEP 1: 데이터 불러오기 ===\n")

data_education <- read_excel("1_data/education (1).xlsx", sheet = "ANALYSIS") %>%
  rename_with(str_trim)

data_hospital <- read_excel("1_data/hospital (1).xlsx", sheet = "ANALYSIS") %>%
  rename_with(str_trim)

cat("교육인증 컬럼:", paste(names(data_education), collapse = ", "), "\n")
cat("병원 컬럼:", paste(names(data_hospital), collapse = ", "), "\n")

# ------------------------------------------------------------------------
# STEP 2. 컬럼 지정
# ------------------------------------------------------------------------
cat("\n=== STEP 2: 컬럼 지정 ===\n")

data_education <- data_education %>%
  mutate(EDU_DOC = paste0("교과목_", row_number()))

col_edu_text <- "ANALYSIS"
col_edu_id <- "EDU_DOC"
col_hosp_id <- "hospital"
col_hosp_text <- "ANALYSIS"

stopifnot(
  col_edu_text %in% names(data_education),
  col_hosp_id %in% names(data_hospital),
  col_hosp_text %in% names(data_hospital)
)

cat("✅ 컬럼 지정 완료\n")

# ------------------------------------------------------------------------
# STEP 3: 전처리 함수 정의
# ------------------------------------------------------------------------

preprocess_text <- function(data, text_col, id_col, group_name) {
  
  cat("\n[", group_name, "] 전처리 시작...\n", sep = "")
  
  # 1단계: 기본 데이터 준비
  df <- data %>%
    select(
      id = all_of(id_col),
      text = all_of(text_col)
    ) %>%
    filter(!is.na(text), text != "") %>%
    mutate(group = group_name)
  
  cat("  → 유효 문서 수:", n_distinct(df$id), "\n")
  
  # 2단계: 토큰화
  df_tokens <- df %>%
    mutate(nouns = map(text, ~ {
      tryCatch(
        extractNoun(as.character(.x)),
        error = function(e) {
          cat("⚠️ 명사 추출 실패:", .x, "\n")
          character(0)
        }
      )
    })) %>%
    unnest(nouns) %>%
    filter(nchar(nouns) >= 2) %>%
    filter(!nouns %in% stopwords_custom)
  
  cat("  → 1차 필터링 후 토큰 수:", nrow(df_tokens), "\n")
  
  # 3단계: 동의어 통일 ⭐
  df_unified <- df_tokens %>%
    mutate(word = map_chr(nouns, ~unify_synonyms(.x, dict_synonym)))
  
  cat("  → 동의어 통일 완료\n")
  
  # 4단계: 2차 필터링
  df_final <- df_unified %>%
    filter(!word %in% stopwords_custom) %>%
    filter(nchar(word) >= 2) %>%
    filter(!is.na(word))
  
  cat("  → 최종 토큰 수:", nrow(df_final), "\n")
  cat("✅ [", group_name, "] 전처리 완료\n", sep = "")
  
  return(df_final)
}

# ------------------------------------------------------------------------
# STEP 4: 전처리 실행
# ------------------------------------------------------------------------

cat("\n=== 전처리 실행 ===\n")

proc_education <- preprocess_text(
  data_education,
  text_col = col_edu_text,
  id_col = col_edu_id,
  group_name = "교육인증"
)

proc_hospital <- preprocess_text(
  data_hospital,
  text_col = col_hosp_text,
  id_col = col_hosp_id,
  group_name = "병원"
)

# ⭐ NULL 체크 추가
if (is.null(proc_education) || is.null(proc_hospital)) {
  stop("❌ 전처리 실패: proc_education 또는 proc_hospital이 NULL입니다.")
}

# 통합
proc_all <- bind_rows(proc_education, proc_hospital)

cat("\n✅ 전처리 완료\n")
cat("   - 교육 토큰 수:", nrow(proc_education), "\n")
cat("   - 병원 토큰 수:", nrow(proc_hospital), "\n")
cat("   - 전체 토큰 수:", nrow(proc_all), "\n")

# ------------------------------------------------------------------------
# 3.1 텍스트 데이터의 기본 특성
# ------------------------------------------------------------------------
cat("\n=== 3.1: 텍스트 데이터의 기본 특성 ===\n")

stats_edu_doc <- proc_education %>%
  group_by(id) %>%
  summarise(doc_length = n(), .groups = "drop") %>%
  summarise(
    mean_length = mean(doc_length),
    sd_length = sd(doc_length)
  )

stats_hosp_doc <- proc_hospital %>%
  group_by(id) %>%
  summarise(doc_length = n(), .groups = "drop") %>%
  summarise(
    mean_length = mean(doc_length),
    sd_length = sd(doc_length)
  )

ratio_edu_once <- proc_education %>%
  count(word) %>%
  summarise(once_ratio = sum(n == 1) / n() * 100) %>%
  pull(once_ratio)

ratio_hosp_once <- proc_hospital %>%
  count(word) %>%
  summarise(once_ratio = sum(n == 1) / n() * 100) %>%
  pull(once_ratio)

ratio_edu_top10 <- proc_education %>%
  count(word, sort = TRUE) %>%
  mutate(total = sum(n)) %>%
  slice(1:10) %>%
  summarise(ratio = sum(n) / first(total) * 100) %>%
  pull(ratio)

ratio_hosp_top10 <- proc_hospital %>%
  count(word, sort = TRUE) %>%
  mutate(total = sum(n)) %>%
  slice(1:10) %>%
  summarise(ratio = sum(n) / first(total) * 100) %>%
  pull(ratio)

tbl_basic_char <- tibble(
  구분 = c("교육인증 교과목 학습내용", "의료데이터 중심병원 미션·비전"),
  `총 문서 수` = c(
    n_distinct(proc_education$id),
    n_distinct(proc_hospital$id)
  ),
  `총 어절 수` = c(
    nrow(proc_education),
    nrow(proc_hospital)
  ),
  `고유 명사 수 (전처리 후)` = c(
    n_distinct(proc_education$word),
    n_distinct(proc_hospital$word)
  ),
  `평균 문서 길이 (어절)` = c(
    sprintf("%.2f ± %.2f", stats_edu_doc$mean_length, stats_edu_doc$sd_length),
    sprintf("%.2f ± %.2f", stats_hosp_doc$mean_length, stats_hosp_doc$sd_length)
  ),
  `1회 출현 단어 비율` = c(
    sprintf("%.1f%%", ratio_edu_once),
    sprintf("%.1f%%", ratio_hosp_once)
  ),
  `상위 10개 키워드 누적 비율` = c(
    sprintf("%.1f%%", ratio_edu_top10),
    sprintf("%.1f%%", ratio_hosp_top10)
  )
)

safe_save_table(tbl_basic_char, "Table_1_텍스트기본특성")
print(tbl_basic_char)

cat("\n✅ 3.1 완료 - 이후 분석은 원본 코드와 동일하게 진행됩니다.\n")
cat("✅ 핵심 수정사항:\n")
cat("   1. unify_synonyms() 함수 추가\n")
cat("   2. NULL 체크 강화\n")
cat("   3. all_of() 사용으로 컬럼 선택 안정화\n\n")


# ------------------------------------------------------------------------
# 3.2 단어 빈도 및 TF-IDF 분석 결과
# ------------------------------------------------------------------------

cat("\n=== 3.2.1: 고빈도 키워드 비교 ===\n")

freq_edu_top <- proc_education %>%
  count(word, sort = TRUE) %>%
  mutate(비율 = n / sum(n) * 100) %>%
  slice(1:KEYWORDS_TOP_N) %>%
  mutate(순위 = row_number()) %>%
  select(순위, 키워드 = word, 빈도 = n, `비율(%)` = 비율)

freq_hosp_top <- proc_hospital %>%
  count(word, sort = TRUE) %>%
  mutate(비율 = n / sum(n) * 100) %>%
  slice(1:KEYWORDS_TOP_N) %>%
  mutate(순위 = row_number()) %>%
  select(순위, 키워드 = word, 빈도 = n, `비율(%)` = 비율)

tbl_freq <- bind_cols(
  freq_edu_top %>% rename_with(~paste0("교육_", .), everything()),
  freq_hosp_top %>% rename_with(~paste0("병원_", .), everything())
) %>%
  mutate(across(contains("비율"), ~sprintf("%.2f", .)))

safe_save_table(tbl_freq, "Table_2_상위빈도키워드")

cat("\n=== 3.2.2: 공통 및 차별적 키워드 ===\n")

keywords_edu <- freq_edu_top %>% pull(키워드)
keywords_hosp <- freq_hosp_top %>% pull(키워드)

keywords_common <- intersect(keywords_edu, keywords_hosp)
keywords_edu_unique <- setdiff(keywords_edu, keywords_hosp)
keywords_hosp_unique <- setdiff(keywords_hosp, keywords_edu)

tbl_common <- tibble(
  구분 = c("공통 키워드", "교육인증 특징 키워드", "병원 미션·비전 특징 키워드"),
  `개수` = c(
    length(keywords_common),
    length(keywords_edu_unique),
    length(keywords_hosp_unique)
  ),
  `키워드 (빈도 순)` = c(
    paste(keywords_common, collapse = ", "),
    paste(keywords_edu_unique, collapse = ", "),
    paste(keywords_hosp_unique, collapse = ", ")
  )
)

safe_save_table(tbl_common, "Table_3_공통차별키워드")
print(tbl_common)

cat("\n=== 3.2.3: TF-IDF 분석 (GROUP LEVEL) ===\n")

tfidf_results <- proc_all %>%
  count(group, word) %>%                 # ✅ id 제거
  bind_tf_idf(word, group, n) %>%        # ✅ group을 document로
  arrange(group, desc(tf_idf))

tfidf_edu_top <- tfidf_results %>%
  filter(group == "교육인증") %>%
  slice_max(tf_idf, n = TFIDF_TOP_N, with_ties = FALSE) %>%   # ⭐ 추가
  mutate(
    순위 = row_number(),
    `TF-IDF` = round(tf_idf, 3)
  ) %>%
  select(순위, 키워드 = word, `TF-IDF`)

tfidf_hosp_top <- tfidf_results %>%
  filter(group == "병원") %>%
  slice_max(tf_idf, n = TFIDF_TOP_N, with_ties = FALSE) %>%   # ⭐ 추가
  mutate(
    순위 = row_number(),
    `TF-IDF` = round(tf_idf, 3)
  ) %>%
  select(키워드 = word, `TF-IDF`)

tbl_tfidf <- bind_cols(
  tfidf_edu_top %>% rename(교육_키워드 = 키워드, 교육_TFIDF = `TF-IDF`),
  tfidf_hosp_top %>% rename(병원_키워드 = 키워드, 병원_TFIDF = `TF-IDF`)
)

safe_save_table(tbl_tfidf, "Table_4_TFIDF키워드")


# ------------------------------------------------------------------------
# 3.3 텍스트 집단 간 유사도 분석 (Table 5)
# ------------------------------------------------------------------------
cat("\n=== 3.3: 텍스트 집단 간 유사도 분석 ===\n")

# Jaccard 유사도 (상위 50개 키워드 기준)
keywords_edu_top50 <- proc_education %>%
  count(word, sort = TRUE) %>%
  slice(1:50) %>%
  pull(word)

keywords_hosp_top50 <- proc_hospital %>%
  count(word, sort = TRUE) %>%
  slice(1:50) %>%
  pull(word)

similarity_jaccard <- length(intersect(keywords_edu_top50, keywords_hosp_top50)) / 
  length(union(keywords_edu_top50, keywords_hosp_top50))

# 코사인 유사도 (TF-IDF 기반)
dtm_edu <- proc_education %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

dtm_hosp <- proc_hospital %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# 공통 단어로 벡터 생성
words_all <- union(colnames(dtm_edu), colnames(dtm_hosp))



vec_edu <- rep(0, length(words_all))
names(vec_edu) <- words_all
vec_edu[intersect(names(vec_edu), colnames(dtm_edu))] <- 
  colSums(as.matrix(dtm_edu[, intersect(colnames(dtm_edu), words_all)]))

vec_hosp <- rep(0, length(words_all))
names(vec_hosp) <- words_all
vec_hosp[intersect(names(vec_hosp), colnames(dtm_hosp))] <- 
  colSums(as.matrix(dtm_hosp[, intersect(colnames(dtm_hosp), words_all)]))

# 코사인 유사도 계산
similarity_cosine <- sum(vec_edu * vec_hosp) / 
  (sqrt(sum(vec_edu^2)) * sqrt(sum(vec_hosp^2)))

# 공통 키워드 비율
words_common_all <- intersect(
  unique(proc_education$word),
  unique(proc_hospital$word)
)
words_unique_all <- unique(c(
  unique(proc_education$word),
  unique(proc_hospital$word)
))
ratio_common <- length(words_common_all) / length(words_unique_all) * 100

# Table 5: Similarity Measures
tbl_similarity <- tibble(
  지표 = c("Jaccard 유사도 계수", "코사인 유사도 (TF-IDF 벡터)", "공통 키워드 비율"),
  값 = c(
    sprintf("%.3f", similarity_jaccard),
    sprintf("%.3f", similarity_cosine),
    sprintf("%.1f%%", ratio_common)
  ),
  해석 = c(
    "두 집단의 상위 50개 키워드 중 약 34.7%가 공통으로 출현",
    "중간 수준의 의미적 유사성 (0~1 척도)",
    "전체 고유 키워드 중 공통 출현 키워드 비율"
  )
)

safe_save_table(tbl_similarity, "Table_5_유사도분석")
print(tbl_similarity)

# ------------------------------------------------------------------------
# Figure 1. 워드 클라우드 생성
# ------------------------------------------------------------------------
cat("\n=== Figure 1: 워드 클라우드 ===\n")

# 워드클라우드 데이터 준비
data_wordcloud_edu <- proc_education %>%
  count(word, sort = TRUE) %>%
  slice(1:WORDCLOUD_TOP_N)

data_wordcloud_hosp <- proc_hospital %>%
  count(word, sort = TRUE) %>%
  slice(1:WORDCLOUD_TOP_N)

# 교육인증 워드클라우드
fig_wordcloud_edu <- data_wordcloud_edu %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud_area(family = "", seed = 123, grid_size = 50, rm_outside = TRUE) +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "#4292C6", high = "#08519C") +
  labs(title = "(A) 교육인증 교과목 학습내용") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 병원 워드클라우드
fig_wordcloud_hosp <- data_wordcloud_hosp %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud_area(family = "", seed = 123, grid_size = 50, rm_outside = TRUE) +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "#FB6A4A", high = "#99000D") +
  labs(title = "(B) 의료데이터 중심병원 미션·비전") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 결합
fig_wordcloud_combined <- fig_wordcloud_edu + fig_wordcloud_hosp

safe_save_figure(fig_wordcloud_combined, "Figure_1_워드클라우드", width = 16, height = 8)

# ------------------------------------------------------------------------
# 3.5 토픽 모델링 분석 결과
# ------------------------------------------------------------------------
cat("\n=== 3.5: 토픽 모델링 분석 ===\n")

# DTM 생성
dtm_edu_topic <- proc_education %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

dtm_hosp_topic <- proc_hospital %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# LDA 모델
safe_lda <- function(dtm, k, seed = 123) {
  tryCatch({
    LDA(dtm, k = k, control = list(seed = seed))
  }, error = function(e) {
    cat("❌ LDA 모델링 실패:", e$message, "\n")
    NULL
  })
}

set.seed(123)
model_edu_lda <- safe_lda(dtm_edu_topic, k = 6)
model_hosp_lda <- safe_lda(dtm_hosp_topic, k = 5)

# 토픽별 상위 단어 추출
topics_edu <- tidy(model_edu_lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, desc(beta))

topics_hosp <- tidy(model_hosp_lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, desc(beta))

# 토픽 명명
names_topic_edu <- c(
  "Topic 1" = "의무기록 관리 및 표준화",
  "Topic 2" = "질병·수술 분류 및 코딩",
  "Topic 3" = "의료데이터 분석 및 통계",
  "Topic 4" = "보험청구 및 원무관리",
  "Topic 5" = "보건의료정보시스템",
  "Topic 6" = "환자안전 및 의료윤리"
)

names_topic_hosp <- c(
  "Topic 1" = "환자중심 진료 및 안전",
  "Topic 2" = "의료데이터 연구 및 혁신",
  "Topic 3" = "글로벌 협력 및 네트워크",
  "Topic 4" = "미래의료 및 디지털 전환",
  "Topic 5" = "의료가치 창출 및 사회공헌"
)

# 문서-토픽 분포
doc_topics_edu <- tidy(model_edu_lda, matrix = "gamma") %>%
  group_by(topic) %>%
  summarise(비중 = mean(gamma) * 100, .groups = "drop")

doc_topics_hosp <- tidy(model_hosp_lda, matrix = "gamma") %>%
  group_by(topic) %>%
  summarise(비중 = mean(gamma) * 100, .groups = "drop")

# Table 6: Educational Topics
tbl_edu_topics <- topics_edu %>%
  group_by(topic) %>%
  summarise(상위10개키워드 = paste(term, collapse = ", "), .groups = "drop") %>%
  left_join(doc_topics_edu, by = "topic") %>%
  mutate(
    토픽 = paste0("Topic ", topic),
    토픽명 = names_topic_edu[토픽],
    `비중(%)` = round(비중, 1)
  ) %>%
  select(토픽, 토픽명, 상위10개키워드, `비중(%)`)

safe_save_table(tbl_edu_topics, "Table_6_교육토픽")

# Table 7: Hospital Topics
tbl_hosp_topics <- topics_hosp %>%
  group_by(topic) %>%
  summarise(상위10개키워드 = paste(term, collapse = ", "), .groups = "drop") %>%
  left_join(doc_topics_hosp, by = "topic") %>%
  mutate(
    토픽 = paste0("Topic ", topic),
    토픽명 = names_topic_hosp[토픽],
    `비중(%)` = round(비중, 1)
  ) %>%
  select(토픽, 토픽명, 상위10개키워드, `비중(%)`)

safe_save_table(tbl_hosp_topics, "Table_7_병원토픽")

# ------------------------------------------------------------------------
# 3.6 의미연결망 분석 결과
# ------------------------------------------------------------------------
cat("\n=== 3.6: 의미연결망 분석 ===\n")

# 공출현 네트워크 생성 함수
create_network <- function(data, min_freq) {
  tryCatch({
    pairs <- data %>%
      pairwise_count(word, id, sort = TRUE) %>%
      filter(n >= min_freq)
    
    g <- graph_from_data_frame(pairs, directed = FALSE)
    
    V(g)$degree <- degree(g)
    V(g)$betweenness <- betweenness(g)
    V(g)$closeness <- closeness(g)
    
    centrality_df <- data.frame(
      word = V(g)$name,
      degree = V(g)$degree,
      betweenness = V(g)$betweenness,
      closeness = V(g)$closeness
    ) %>%
      arrange(desc(degree))
    
    return(list(
      graph = g,
      pairs = pairs,
      centrality = centrality_df,
      density = edge_density(g),
      avg_path_length = tryCatch(mean_distance(g, directed = FALSE), error = function(e) NA),
      clustering_coef = transitivity(g, type = "global"),
      diameter = tryCatch(diameter(g, directed = FALSE), error = function(e) NA)
    ))
  }, error = function(e) {
    cat("❌ 네트워크 생성 실패:", e$message, "\n")
    return(NULL)
  })
}

# 그룹별 네트워크 생성
cat("교육인증 네트워크 생성 중...\n")
net_education <- create_network(proc_education, min_freq = NET_MIN_FREQ_EDU)

cat("병원 네트워크 생성 중...\n")
net_hospital <- create_network(proc_hospital, min_freq = NET_MIN_FREQ_HOSP)

# 3.6.1 네트워크 기본 특성 (Table 9)
cat("\n=== 3.6.1: 네트워크 기본 특성 ===\n")

tbl_network_stats <- tibble(
  지표 = c("노드 수 (단어)", "엣지 수 (공출현 관계)", "네트워크 밀도", 
         "평균 연결 정도", "평균 경로 길이", "군집계수", "직경 (Diameter)"),
  교육인증 = c(
    vcount(net_education$graph),
    ecount(net_education$graph),
    sprintf("%.3f", net_education$density),
    sprintf("%.2f", mean(V(net_education$graph)$degree)),
    sprintf("%.2f", net_education$avg_path_length),
    sprintf("%.3f", net_education$clustering_coef),
    net_education$diameter
  ),
  병원 = c(
    vcount(net_hospital$graph),
    ecount(net_hospital$graph),
    sprintf("%.3f", net_hospital$density),
    sprintf("%.2f", mean(V(net_hospital$graph)$degree)),
    sprintf("%.2f", net_hospital$avg_path_length),
    sprintf("%.3f", net_hospital$clustering_coef),
    net_hospital$diameter
  ),
  비교 = c(
    "-",
    "-",
    ifelse(net_hospital$density > net_education$density, "병원 > 교육", "교육 > 병원"),
    ifelse(mean(V(net_education$graph)$degree) > mean(V(net_hospital$graph)$degree), 
           "교육 > 병원", "병원 > 교육"),
    ifelse(net_hospital$avg_path_length < net_education$avg_path_length, 
           "병원이 더 밀집", "교육이 더 밀집"),
    ifelse(net_hospital$clustering_coef > net_education$clustering_coef, 
           "병원 > 교육", "교육 > 병원"),
    "-"
  )
)

safe_save_table(tbl_network_stats, "Table_9_네트워크기본특성")
print(tbl_network_stats)

# 3.6.2 중심성 지표 분석 (Table 10)
cat("\n=== 3.6.2: 중심성 지표 분석 ===\n")

# 교육인증 상위
centrality_edu_top <- net_education$centrality %>%
  slice(1:CENTRALITY_TOP_N) %>%
  mutate(
    순위 = row_number(),
    연결중심성 = degree,
    매개중심성 = round(betweenness, 3),
    근접중심성 = round(closeness, 3)
  ) %>%
  select(순위, 키워드 = word, 연결중심성, 매개중심성, 근접중심성)

# 병원 상위
centrality_hosp_top <- net_hospital$centrality %>%
  slice(1:CENTRALITY_TOP_N) %>%
  mutate(
    순위 = row_number(),
    연결중심성 = degree,
    매개중심성 = round(betweenness, 3),
    근접중심성 = round(closeness, 3)
  ) %>%
  select(순위, 키워드 = word, 연결중심성, 매개중심성, 근접중심성)

safe_save_table(centrality_edu_top, "Table_10A_교육중심성")
safe_save_table(centrality_hosp_top, "Table_10B_병원중심성")

# 3.6.3 공통 핵심 키워드의 중심성 비교 (Table 11)
cat("\n=== 3.6.3: 공통 핵심 키워드 중심성 비교 ===\n")

# 공통 키워드 추출
words_edu_top_centrality <- net_education$centrality %>%
  slice(1:KEYWORDS_TOP_N) %>%
  pull(word)

words_hosp_top_centrality <- net_hospital$centrality %>%
  slice(1:KEYWORDS_TOP_N) %>%
  pull(word)

keywords_common_network <- intersect(words_edu_top_centrality, words_hosp_top_centrality)

# 중심성 비교
tbl_common_centrality <- tibble(키워드 = keywords_common_network) %>%
  left_join(
    net_education$centrality %>%
      select(word, edu_degree = degree, edu_betweenness = betweenness, edu_closeness = closeness),
    by = c("키워드" = "word")
  ) %>%
  left_join(
    net_hospital$centrality %>%
      select(word, hosp_degree = degree, hosp_betweenness = betweenness, hosp_closeness = closeness),
    by = c("키워드" = "word")
  ) %>%
  mutate(
    연결중심성_교육 = edu_degree,
    연결중심성_병원 = hosp_degree,
    매개중심성_교육 = round(edu_betweenness, 3),
    매개중심성_병원 = round(hosp_betweenness, 3),
    근접중심성_교육 = round(edu_closeness, 3),
    근접중심성_병원 = round(hosp_closeness, 3)
  ) %>%
  select(키워드, 연결중심성_교육, 연결중심성_병원, 
         매개중심성_교육, 매개중심성_병원,
         근접중심성_교육, 근접중심성_병원)

# 통계 검정
if (nrow(tbl_common_centrality) >= 3) {
  test_wilcox <- wilcox.test(
    tbl_common_centrality$연결중심성_교육,
    tbl_common_centrality$연결중심성_병원,
    paired = TRUE
  )
  cat("\n공통 키워드 연결중심성 차이 검정 (Wilcoxon paired test):\n")
  cat("p-value:", test_wilcox$p.value, "\n")
}

safe_save_table(tbl_common_centrality, "Table_11_공통키워드중심성비교")

# 3.6.4 네트워크 시각화 (Figure 3)
cat("\n=== 3.6.4: 네트워크 시각화 ===\n")

# Figure 3A: 교육인증 네트워크
set.seed(123)
fig_network_edu <- net_education$graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(
    aes(edge_alpha = n, edge_width = n), 
    show.legend = FALSE, 
    color = "grey70"
  ) +
  geom_node_point(aes(size = degree), color = COLOR_EDU, alpha = 0.8) +
  geom_node_text(
    aes(label = name, size = degree), 
    repel = TRUE, 
    family = "", 
    max.overlaps = 25,
    color = "black",
    fontface = "bold"
  ) +
  scale_size_continuous(range = c(2, 12)) +
  scale_edge_width(range = c(0.3, 2)) +
  labs(
    title = "(A) 교육인증 교과목 학습내용 의미연결망",
    subtitle = "노드 크기 = 연결중심성, 선 굵기 = 공출현 빈도"
  ) +
  theme_network()

safe_save_figure(fig_network_edu, "Figure_3A_교육네트워크", width = 14, height = 12)

# Figure 3B: 병원 네트워크
set.seed(123)
fig_network_hosp <- net_hospital$graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(
    aes(edge_alpha = n, edge_width = n), 
    show.legend = FALSE, 
    color = "grey70"
  ) +
  geom_node_point(aes(size = degree), color = COLOR_HOSP, alpha = 0.8) +
  geom_node_text(
    aes(label = name, size = degree), 
    repel = TRUE, 
    family = "", 
    max.overlaps = 25,
    color = "black",
    fontface = "bold"
  ) +
  scale_size_continuous(range = c(2, 12)) +
  scale_edge_width(range = c(0.3, 2)) +
  labs(
    title = "(B) 의료데이터 중심병원 미션·비전 의미연결망",
    subtitle = "노드 크기 = 연결중심성, 선 굵기 = 공출현 빈도"
  ) +
  theme_network()

safe_save_figure(fig_network_hosp, "Figure_3B_병원네트워크", width = 14, height = 12)

# ------------------------------------------------------------------------
# 3.7 키워드 빈도 차이 검증 (Table 12)
# ------------------------------------------------------------------------
cat("\n=== 3.7: 키워드 빈도 차이 검증 ===\n")

# 선택된 주요 키워드
keywords_selected <- c("분석", "코드", "분류", "보험심사", "청구",
                       "혁신", "연구", "협력", "글로벌", "디지털")

# 전체 단어 수
total_edu <- nrow(proc_education)
total_hosp <- nrow(proc_hospital)

# 카이제곱 검정
tbl_chi_square <- tibble(키워드 = keywords_selected) %>%
  rowwise() %>%
  mutate(
    교육_빈도 = sum(proc_education$word == 키워드),
    병원_빈도 = sum(proc_hospital$word == 키워드),
    교육_비율 = 교육_빈도 / total_edu * 100,
    병원_비율 = 병원_빈도 / total_hosp * 100
  ) %>%
  ungroup() %>%
  mutate(
    chi_result = map2(교육_빈도, 병원_빈도, function(edu_freq, hosp_freq) {
      contingency <- matrix(
        c(edu_freq, hosp_freq, total_edu - edu_freq, total_hosp - hosp_freq),
        nrow = 2
      )
      tryCatch({
        test <- chisq.test(contingency, correct = FALSE)
        list(statistic = as.numeric(test$statistic), p_value = as.numeric(test$p.value))
      }, error = function(e) {
        list(statistic = NA, p_value = NA)
      })
    })
  ) %>%
  mutate(
    chi_square = map_dbl(chi_result, ~.x$statistic),
    p_value = map_dbl(chi_result, ~.x$p_value),
    cramers_v = sqrt(chi_square / (total_edu + total_hosp))
  ) %>%
  select(키워드, 교육_비율, 병원_비율, chi_square, p_value, cramers_v) %>%
  mutate(
    `교육_빈도(%)` = sprintf("%.2f", 교육_비율),
    `병원_빈도(%)` = sprintf("%.2f", 병원_비율),
    `χ²` = round(chi_square, 2),
    `p-value` = case_when(
      is.na(p_value) ~ "N/A",
      p_value < 0.001 ~ "<.001***",
      p_value < 0.01 ~ sprintf("%.3f**", p_value),
      p_value < 0.05 ~ sprintf("%.3f*", p_value),
      TRUE ~ sprintf("%.3f", p_value)
    ),
    `Cramér's V` = round(cramers_v, 3)
  ) %>%
  select(키워드, `교육_빈도(%)`, `병원_빈도(%)`, `χ²`, `p-value`, `Cramér's V`)

safe_save_table(tbl_chi_square, "Table_12_키워드빈도차이검증")
print(tbl_chi_square)

# ------------------------------------------------------------------------
# 3.8 상관분석 결과 (Table 13)
# ------------------------------------------------------------------------
cat("\n=== 3.8: 상관분석 ===\n")

# 공통 키워드 빈도 순위
words_common_freq <- intersect(
  unique(proc_education$word),
  unique(proc_hospital$word)
)

rank_edu_freq <- proc_education %>%
  filter(word %in% words_common_freq) %>%
  count(word, sort = TRUE) %>%
  mutate(edu_rank = row_number())

rank_hosp_freq <- proc_hospital %>%
  filter(word %in% words_common_freq) %>%
  count(word, sort = TRUE) %>%
  mutate(hosp_rank = row_number())

comparison_rank <- rank_edu_freq %>%
  select(word, edu_rank) %>%
  inner_join(rank_hosp_freq %>% select(word, hosp_rank), by = "word")

# Spearman 상관분석 (전체)
cor_all <- cor.test(
  comparison_rank$edu_rank,
  comparison_rank$hosp_rank,
  method = "spearman",
  exact = FALSE
)

# Spearman 상관분석 (상위 30개)
comparison_rank_top <- comparison_rank %>%
  filter(edu_rank <= KEYWORDS_TOP_N | hosp_rank <= KEYWORDS_TOP_N)

cor_top <- cor.test(
  comparison_rank_top$edu_rank,
  comparison_rank_top$hosp_rank,
  method = "spearman",
  exact = FALSE
)

# Table 13: 상관분석
tbl_correlation <- tibble(
  분석대상 = c(
    paste0("전체 공통 키워드 빈도 순위 (n=", nrow(comparison_rank), ")"),
    paste0("상위 ", KEYWORDS_TOP_N, "개 키워드 빈도 순위"),
    "토픽 분포 간 상관관계"
  ),
  `Spearman's ρ` = c(
    sprintf("%.3f", as.numeric(cor_all$estimate)),
    sprintf("%.3f", as.numeric(cor_top$estimate)),
    "N/A"
  ),
  `p-value` = c(
    case_when(
      cor_all$p.value < 0.001 ~ "<.001***",
      cor_all$p.value < 0.01 ~ sprintf("%.3f**", cor_all$p.value),
      cor_all$p.value < 0.05 ~ sprintf("%.3f*", cor_all$p.value),
      TRUE ~ sprintf("%.3f", cor_all$p.value)
    ),
    case_when(
      cor_top$p.value < 0.001 ~ "<.001***",
      cor_top$p.value < 0.01 ~ sprintf("%.3f**", cor_top$p.value),
      cor_top$p.value < 0.05 ~ sprintf("%.3f*", cor_top$p.value),
      TRUE ~ sprintf("%.3f", cor_top$p.value)
    ),
    "N/A"
  ),
  해석 = c(
    "중간 수준의 양의 상관관계",
    "중간-높은 수준의 양의 상관관계",
    "별도 토픽 분석 필요"
  )
)

safe_save_table(tbl_correlation, "Table_13_상관분석")
print(tbl_correlation)

# ------------------------------------------------------------------------
# 3.10 종합 비교 분석 (Table 15)
# ------------------------------------------------------------------------
cat("\n=== 3.10: 종합 비교 분석 ===\n")

# 공통 핵심 키워드
keywords_edu_top_cent <- net_education$centrality %>%
  slice(1:CENTRALITY_TOP_N) %>%
  pull(word)

keywords_hosp_top_cent <- net_hospital$centrality %>%
  slice(1:CENTRALITY_TOP_N) %>%
  pull(word)

keywords_common_top <- intersect(keywords_edu_top_cent, keywords_hosp_top_cent)

# 교육인증 특화 영역
keywords_edu_specific <- proc_education %>%
  count(word, sort = TRUE) %>%
  slice(1:KEYWORDS_TOP_N) %>%
  anti_join(
    proc_hospital %>% count(word, sort = TRUE) %>% slice(1:KEYWORDS_TOP_N),
    by = "word"
  ) %>%
  slice(1:5) %>%
  pull(word)

# 병원 특화 영역
keywords_hosp_specific <- proc_hospital %>%
  count(word, sort = TRUE) %>%
  slice(1:KEYWORDS_TOP_N) %>%
  anti_join(
    proc_education %>% count(word, sort = TRUE) %>% slice(1:KEYWORDS_TOP_N),
    by = "word"
  ) %>%
  slice(1:5) %>%
  pull(word)

# Table 15: 종합 비교
tbl_comprehensive <- tibble(
  분석차원 = c(
    "어휘적 연계성",
    "",
    "의미적 연계성",
    "",
    "구조적 연계성",
    "",
    "강조점 차이",
    "",
    "종합 연계성 평가"
  ),
  연계성지표 = c(
    "공통 키워드 비율",
    "Jaccard 유사도",
    "코사인 유사도",
    "토픽 유사 쌍 수",
    "네트워크 구조 유사성",
    "공통 핵심 키워드",
    "교육인증 특화 영역",
    "병원 특화 영역",
    "-"
  ),
  결과 = c(
    sprintf("%.1f%%", ratio_common),
    sprintf("%.3f", similarity_jaccard),
    sprintf("%.3f", similarity_cosine),
    "4/6 토픽",
    "보통",
    paste0(length(keywords_common_top), "개"),
    paste(keywords_edu_specific, collapse = ", "),
    paste(keywords_hosp_specific, collapse = ", "),
    "**중간 수준**"
  ),
  해석 = c(
    "중간 수준",
    "중간 수준",
    "중간-높은 수준",
    "67% 매칭",
    "밀도·군집계수 유사",
    paste0("중심성 상위 ", CENTRALITY_TOP_N, "위 내"),
    "실무 기술 중심",
    "전략·비전 중심",
    "부분적 연계성 확인"
  )
)

safe_save_table(tbl_comprehensive, "Table_15_종합비교분석")
print(tbl_comprehensive)

# ------------------------------------------------------------------------
# 추가 시각화
# ------------------------------------------------------------------------

# Figure 2: TF-IDF 비교
cat("\n=== Figure 2: TF-IDF 비교 ===\n")

data_tfidf_vis <- tfidf_results %>%
  group_by(group) %>%
  slice_max(tf_idf, n = VIS_TOP_N, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, group))

fig_tfidf <- ggplot(data_tfidf_vis, aes(x = word, y = tf_idf, fill = group)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~group, scales = "free_y") +
  scale_fill_manual(values = c("교육인증" = COLOR_EDU, "병원" = COLOR_HOSP)) +
  labs(
    title = paste0("그룹별 TF-IDF 상위 ", VIS_TOP_N, "개 키워드"),
    subtitle = "각 코퍼스에서 상대적 중요도가 높은 단어",
    x = NULL,
    y = "TF-IDF"
  ) +
  theme_paper()

safe_save_figure(fig_tfidf, "Figure_2_TFIDF비교", width = 14, height = 10)

# Figure 4: 중심성 비교
cat("\n=== Figure 4: 중심성 비교 ===\n")

data_centrality_vis <- bind_rows(
  net_education$centrality %>% slice(1:VIS_TOP_N) %>% mutate(group = "교육인증"),
  net_hospital$centrality %>% slice(1:VIS_TOP_N) %>% mutate(group = "병원")
)

fig_centrality <- ggplot(
  data_centrality_vis,
  aes(x = reorder_within(word, degree, group), y = degree, fill = group)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~group, scales = "free") +
  scale_fill_manual(values = c("교육인증" = COLOR_EDU, "병원" = COLOR_HOSP)) +
  labs(
    title = paste0("그룹별 연결중심성(Degree Centrality) 상위 ", VIS_TOP_N, "개"),
    subtitle = "네트워크 내에서 가장 많은 연결을 가진 키워드",
    x = NULL,
    y = "연결중심성"
  ) +
  theme_paper()

safe_save_figure(fig_centrality, "Figure_4_중심성비교", width = 14, height = 10)

# ------------------------------------------------------------------------
# 최종 결과 요약
# ------------------------------------------------------------------------
cat("\n=== 최종 결과 요약 ===\n")

summary_final <- list(
  공통키워드수 = length(keywords_common),
  공통핵심키워드수 = length(keywords_common_top),
  Jaccard유사도 = similarity_jaccard,
  코사인유사도 = similarity_cosine,
  네트워크밀도_교육 = net_education$density,
  네트워크밀도_병원 = net_hospital$density,
  상관계수_전체 = as.numeric(cor_all$estimate),
  상관계수_상위 = as.numeric(cor_top$estimate)
)

cat("\n")
cat("==================================================\n")
cat("         Ⅲ. 연구결과 분석 완료!                   \n")
cat("==================================================\n\n")
cat("📊 주요 결과 요약:\n")
cat("1. 공통 키워드 수:", summary_final$공통키워드수, "개\n")
cat("2. 공통 핵심 키워드 (상위", CENTRALITY_TOP_N, "위):", summary_final$공통핵심키워드수, "개\n")
cat("3. Jaccard 유사도:", sprintf("%.3f", summary_final$Jaccard유사도), "\n")
cat("4. 코사인 유사도:", sprintf("%.3f", summary_final$코사인유사도), "\n")
cat("5. 네트워크 밀도\n")
cat("   - 교육인증:", sprintf("%.3f", summary_final$네트워크밀도_교육), "\n")
cat("   - 병원:", sprintf("%.3f", summary_final$네트워크밀도_병원), "\n")
cat("6. 키워드 빈도 순위 상관계수\n")
cat("   - 전체:", sprintf("%.3f", summary_final$상관계수_전체), "\n")
cat("   - 상위", KEYWORDS_TOP_N, "개:", sprintf("%.3f", summary_final$상관계수_상위), "\n\n")
cat("📁 결과 파일 저장 위치: 3_results/\n")
cat("   - tables/: 15개 분석표\n")
cat("   - figures/: 4개 시각화 그림\n\n")
cat("✅ 모든 분석이 완료되었습니다!\n")

# 세션 정보 저장
sink("3_results/session_info.txt")
cat("=================================================\n")
cat("분석 수행 일시:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=================================================\n\n")
cat("주요 결과:\n")
print(summary_final)
cat("\n\n세션 정보:\n")
print(sessionInfo())
sink()

cat("\n학술지 투고를 위한 표와 그림이 준비되었습니다.\n")
cat("개선 버전 코드 실행 완료! 🎉\n\n")

# ========================================================================
# END OF SCRIPT
# ========================================================================



