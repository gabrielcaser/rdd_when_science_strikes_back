*********************************************************************************************
* Description: This program runs all possible regressions and save their estimates in a excel sheet.

**********************************************************************************************

**********************************************************************************************
* Initial Commands
**********************************************************************************************

cap log close                       /* closes open log files */
set more off                        /* tells Stata not to pause after each step of calculation */
clear                               /* clears current memory */
set memory 500m                     /* increases available memory */

global work_dir 			 = "C:\Users\gabri\OneDrive\Gabriel\Insper\Tese\Engenheiros\replication_code\rdd_when_science_strikes_back\regressions_all"
global output_dir       	 = "C:\Users\gabri\OneDrive\Gabriel\Insper\Tese\Engenheiros\replication_code\rdd_when_science_strikes_back\regressions_all\output"
global temp_dir       	     = "C:\Users\gabri\OneDrive\Gabriel\Insper\Tese\Engenheiros\replication_code\rdd_when_science_strikes_back\regressions_all\output\temp"
global cria_base_tratada_dir = "C:\Users\gabri\OneDrive\Gabriel\Insper\Tese\Engenheiros\replication_code\rdd_when_science_strikes_back\6_create_rdd_dataset\output\data"


cap mkdir "$output_dir\data" // cria pastas
cap mkdir "$temp_dir"	
cap mkdir "$output_dir\logs"

log using "$output_dir\logs\regressions_all.log", text replace

//sample .1

**********************************************************************************************
* Abrindo a base
**********************************************************************************************
global i = 0 // contador de modelos
foreach stem_definition in "strict" "broad" {
	
	foreach var_y in "y_hosp" "y_deaths_sivep" {	
		
		* abrindo base
		import delimited "$cria_base_tratada_dir\rdd_data_moderation_`stem_definition'definition.csv", clear
		
		* regs
		reg `var_y' x t t_x, robust
		
		global i = $i + 1
		* saving results
		if $i == 1 {
			estimates store temp_results
			outreg2 using "$output_dir\data\regression_results.xls",  replace
		}
		else {
			estimates store temp_results
			outreg2 using "$output_dir\data\regression_results.xls",  append
		}
	}
}



**********************************************************************************************
* Código principal
**********************************************************************************************

* regs





* Definindo covariadas que poderão ser usadas


local covariates "idade negros homens n_moradores_t0 i.renda_domicilio_t0 i.nivel_escolaridade cursos_profissionais primeiro_trabalho_t0 tx_ocupacao_t0 tx_formalizacao_ipt_t0 tx_formalizacao_mi_t0 i.n_entrevistas_ipt_t0 n_entrevistas_mi_t0 i.posicao_ocupacao_ipt_t0 posicao_ocupacao_mi_t0 i.estudante"	
sum `covariates' if atrito_t2 == 0

* testes
reg tx_ocupacao_t2 tratamento `covariates' 

ivreg2 tx_ocupacao_t2 (tratamento_completo_t2 = tratamento) `covariates' , first

* Renda

global i = 0 // contador de modelos


// Definir os valores iniciais 


foreach t in "t1" "t2" {
	
	
	** definindo taxa de atrito
	sum atrito_`t'
	local atrito  = `r(mean)' // atrito no follow-up 1 ou 2
	
	** definindo taxa de não-conformidade
	sum tratamento_completo_`t' if atrito_`t' == 0 & tratamento == 1
	local takeup_treat   = `r(mean)' // não conformidade do tratamento
	sum tratamento_completo_`t' if atrito_`t' == 0 & tratamento == 0
	local takeup_control = `r(mean)'
	local tu_rate = (`takeup_treat' - `takeup_control') // taxa de não conformidade total


	// Iniciar loop com diferentes valores para efeito_sd
	foreach dp of numlist 0.05(0.05)1.00 {
		
		foreach teste in "onesided" "" {
			
			foreach nratio in 1 {
				
				local efeito_sd = `dp' // efeito em desvios padrões
				
				foreach alfa in 0.05 0.10 {
					
					foreach var in "renda_trabalho_t0" "log_renda_trabalho_t0" "renda_trabalho_semna_t0" "tx_ocupacao_t0" "empregados_formais_t0" "tx_formalizacao_t0" {
						
						foreach covs in "yes" "no" {
						
							regress `var' `covariates' // regredindo para pegar a variação não correlacionada com as covariadas												    
							local res_sd =round(sqrt(`e(rss)'/`e(df_r)'),0.0001)	//this is the new standard deviation for the power calculation or the residual sd not explained by the control(s). 
						
							sum `var' // gerando valores da variável de interesse para serem considerados no teste de poder
							
							// Definindo parâmetros com base nas estatísticas da variável de interesse
						
							local m1 = `r(mean)' // média da variável na linha de base
							local sd = `r(sd)' 
							local efeito_sd_tu = `dp' * `tu_rate' // efeito em desvios padrões considerando não conformidade
							local efeito_uni = `efeito_sd' * `sd'
							local efeito_uni_tu = `efeito_uni' * `tu_rate' // efeito considerando a não-conformidade
							local m2 = `m1' + `efeito_uni' // média do tratamento (média da base + efeito média esperado)
							local m2_tu =  `m1' + `efeito_uni_tu' // média do tratamento considerando a take-up rate
							
							
							// Gerando tabela com variáveis resultantes do cálculo de poder
							
							if ("`var'" == "renda_trabalho_t0" | "`var'" == "renda_trabalho_semna_t0" | "`var'" == "log_renda_trabalho_t0") & "`covs'" == "yes" { // define que vai usar o metodo twomeans e o erro padrão residual quando variavel for renda e tiver covariadas
								local metodo = "twomeans"
								local sd_parametro = "sd(`res_sd')"
							}
							
							else if ("`var'" == "renda_trabalho_t0" | "`var'" == "renda_trabalho_semna_t0" | "`var'" == "log_renda_trabalho_t0") & "`covs'" == "no" {
								local metodo = "twomeans"
								local sd_parametro = "sd(`sd')"
							}
							
							else { // métodos twoproportions quando for variavel binaria (ocupacao e formalidade)
								local metodo = "twoproportions"
								local sd_parametro = ""
							}
							
							power `metodo' `m1' `m2', power(.8) `sd_parametro' alpha(`alfa') nratio(`nratio') `teste' table // N sem considerar take-up rate
							
							local N = `r(N)'
							
							power `metodo' `m1' `m2_tu', power(.8) `sd_parametro' alpha(`alfa') nratio(`nratio') `teste' table saving("$temp_dir\temp.dta", replace) // N considerando take-up rate
						
							// Calcular o número necessário de observações após o atrito
							local n_necessario_atr = (`r(N)') / (1 - `atrito') 
							
							global i = $i + 1 // contador de modelos
							// Adicionando variáveis na tabela e renomeando
							preserve
								use "$temp_dir\temp.dta", clear
								gen variavel = "`var'"
								gen imd_sd_LATE = `efeito_sd_tu'          // minimo efeito detectavel em desvio padrões considerando não conformidade -> ou seja, é o efeito local na população que de fato recebeu o tratamento (LATE)
								gen imd_sd_ITT = `efeito_sd'              // minimo efeito detectavel em desvio padrões sem considerar não conformidade. Ou seja, é o efeito da Intenção de Tratar, não importando quem de fato seguiu o resultado do sorteio ou não
								gen imd_uni_ITT = `efeito_uni'            // minimo efeito detectavel sem considerar não conformidade
								gen imd_uni_LATE = `efeito_uni_tu'        // minimo efeito detectavel na unidade da variavel considerando não-conformidade
								gen tu_rate = `tu_rate'
								gen atrito = `atrito'
								ren N N_tu                                // N necessário considerando não conformidade (take-up rate)
								gen N_tu_atr = trunc(`n_necessario_atr') // acho que não faz sentido isso -> coloca em número inteiro. N necessário considerando não conformidade e atrito
								gen N = `N'
								gen teste = "`teste'"
								gen covariadas = "`covs'"
								//gen n_ratio = `nratio'
								gen tempo = "`t'"
								replace teste = "twosided" if teste == ""
								gen     cenario = 1  if alpha == 0.05 & teste == "twosided" & nratio == 1 // substituindo por cenários definidos pela Oppen
								replace cenario = 2  if alpha == 0.05 & teste == "onesided" & nratio == 1
								replace cenario = 3  if alpha == 0.10 & teste == "onesided" & nratio == 1
								//replace cenario = 4  if alpha == 0.10 & teste == "onesided" & nratio == 0.5
								replace cenario = 5  if alpha == 0.10 & teste == "twosided" & nratio == 1
								cap drop N1 N2 delta
								cap drop m1 m2 sd
								cap drop p1 p2 
								
								
								order tempo variavel imd_sd_ITT imd_uni_ITT imd_sd_LATE imd_uni_LATE cenario N_tu_atr N_tu N teste nratio covariadas // ordenando vars
								
								
								
								if $i == 1 {
									save "$temp_dir/power_test.dta", replace	 // criando o arquivo
								}
								else {
									append using "$temp_dir/power_test.dta" // apendando 
									sort  tempo variavel imd_sd_ITT cenario covariadas
									drop if cenario == .
									
									save "$temp_dir/power_test.dta", replace // salvando o arquivo consolidade em dta
									export excel using "$output_dir/power_test.xlsx", sheet("testes_poder") sheetreplace firstrow (variables) keepcellfmt // salvando em excel
								}
							
							restore
						}
					}
					
				
				}
		
			}
		
		}
	
	}
}

