#CODE FOR FILTERING EI SCORE BY DIRECTORITE AND PERSONAL VARIABLES
#set working directory
setwd( "M:/Policy Directorate/Policy/252 Project resource & Development/People survey/2016 info/Analysis/OCA analysis/Data files")
#readin PS data
PS<-read.csv('mergeddatav5-EB.csv')
dirs<-c('DH0201','DH0202','DH0203','DH0204','DH0205','DH0225','DH0168')
variable<-c('ees','mw_p','op_p','lm_p','mt_p','ld_p','if_p','rw_p','pb_p','lc_p','ppE03')
for (dir in dirs){
	filtered<-subset(PS,PS$dircodef == dir)
	if (dir == 'DH0201'){
		dir<-'Global & Public Health'
		GPH<-c()
		for (x in 1:11){
			GPH[x]<-mean(filtered[[variable[x]]], na.rm=TRUE)
		}
		GPH[12]<-nrow(filtered)
	}
	if (dir == 'DH0202'){
		dir<-'Finance & Group Operations'
		FGO<-c()
		for (x in 1:11){
			FGO[x]<-mean(filtered[[variable[x]]], na.rm=TRUE)
		}
		FGO[12]<-nrow(filtered)
	}
	if (dir == 'DH0203'){
		dir<-'Chief Medical Officer'
		CMO<-c()
		for (x in 1:11){
			CMO[x]<-mean(filtered[[variable[x]]], na.rm=TRUE)
		}
		CMO[12]<-nrow(filtered)
	}
	if (dir == 'DH0204'){
		dir<-'Community Care'
		CC<-c()
		for (x in 1:11){
			CC[x]<-mean(filtered[[variable[x]]], na.rm=TRUE)
		}
		CC[12]<-nrow(filtered)
	}
	if (dir == 'DH0205'){
		dir<-'Acute Care & Workforce'
		ACW<-c()
		for (x in 1:11){
			ACW[x]<-mean(filtered[[variable[x]]], na.rm=TRUE)
		}
		ACW[12]<-nrow(filtered)
	}	
	if (dir == 'DH0225'){
		dir<-'Chief Scientific Adviser'
		CSA<-c()
		for (x in 1:11){
			CSA[x]<-mean(filtered[[variable[x]]], na.rm=TRUE)
		}
		CSA[12]<-nrow(filtered)
	}
	if (dir == 'DH0168'){
		dir<-'Other'
		O<-c()
		for (x in 1:11){
			O[x]<-mean(filtered[[variable[x]]], na.rm=TRUE)
		}
		O[12]<-nrow(filtered)
	}
#filter by age group, gender, sexual orientation, ethnicity, other care responsibility, child care responsibility, location, grade, working pattern, religion, scs
	filter<-'working pattern'
	if (filter == 'age group'){
		filtered2<-subset(filtered,filtered$agegroup == '24 and under')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$agegroup == '25-29')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$agegroup == '30-34')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)	
		filtered2<-subset(filtered,filtered$agegroup == '35-39')
		D<-c()
		for (x in 1:11){
			D[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		D[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$agegroup == '40-44')
		E<-c()
		for (x in 1:11){
			E[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		E[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$agegroup == '45-49')
		F<-c()
		for (x in 1:11){
			F[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		F[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$agegroup == '50-54')
		G<-c()
		for (x in 1:11){
			G[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		G[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$agegroup == '55-59')
		H<-c()
		for (x in 1:11){
			H[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		H[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$agegroup == '60 and over')
		I<-c()
		for (x in 1:11){
			I[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		I[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1],D[1],E[1],F[1],G[1],H[1],I[1])
		MyWork<-c(A[2],B[2],C[2],D[2],E[2],F[2],G[2],H[2],I[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3],D[3],E[3],F[3],G[3],H[3],I[3])
		MyManager<-c(A[4],B[4],C[4],D[4],E[4],F[4],G[4],H[4],I[4])
		MyTeam<-c(A[5],B[5],C[5],D[5],E[5],F[5],G[5],H[5],I[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6],D[6],E[6],F[6],G[6],H[6],I[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7],D[7],E[7],F[7],G[7],H[7],I[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8],D[8],E[8],F[8],G[8],H[8],I[8])
		PayAndBenifits<-c(A[9],B[9],C[9],D[9],E[9],F[9],G[9],H[9],I[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10],D[10],E[10],F[10],G[10],H[10],I[10])
		BullingAndHarasment<-c(A[11],B[11],C[11],D[11],E[11],F[11],G[11],H[11],I[11])
		N<-c(A[12],B[12],C[12],D[12],E[12],F[12],G[12],H[12],I[12])
		agegroups<-c('24 and under','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60 and over')
		dfei<-data.frame(agegroups,EI,N)
		dfts<-data.frame(agegroups,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(agegroups,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
}
	if (filter == 'gender'){
		filtered2<-subset(filtered,filtered$J01 == 'Male')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J01 == 'Female')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J01 == 'Prefer not to say')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1])
		MyWork<-c(A[2],B[2],C[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3])
		MyManager<-c(A[4],B[4],C[4])
		MyTeam<-c(A[5],B[5],C[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8])
		PayAndBenifits<-c(A[9],B[9],C[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10])
		BullingAndHarasment<-c(A[11],B[11],C[11])
		N<-c(A[12],B[12],C[12])
		gender<-c('Male','Female','Prefer not to say')
		dfei<-data.frame(gender,EI,N)
		dfts<-data.frame(gender,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(gender,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}	
		if (filter == 'sexual orientation'){
		filtered2<-subset(filtered,filtered$J07 == 'Bisexual')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J07 == 'Gay or Lesbian')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$J07 == 'Hetrosexual/straight')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)	
		filtered2<-subset(filtered,filtered$J07 == 'Other')
		D<-c()
		for (x in 1:11){
			D[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		D[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$J07 == 'Prefer not to say')
		E<-c()
		for (x in 1:11){
			E[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		E[12]<-nrow(filtered2)		
		EI<-c(A[1],B[1],C[1],D[1],E[1])
		MyWork<-c(A[2],B[2],C[2],D[2],E[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3],D[3],E[3])
		MyManager<-c(A[4],B[4],C[4],D[4],E[4])
		MyTeam<-c(A[5],B[5],C[5],D[5],E[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6],D[6],E[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7],D[7],E[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8],D[8],E[8])
		PayAndBenifits<-c(A[9],B[9],C[9],D[9],E[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10],D[10],E[10])
		BullingAndHarasment<-c(A[11],B[11],C[11],D[11],E[11])
		N<-c(A[12],B[12],C[12],D[12],E[12])
		sexualorientation<-c('Bisexual','Gay or Lesbian','Hetrosexual/straight','Other','Prefer not to say')
		dfei<-data.frame(sexualorientation,EI,N)
		dfts<-data.frame(sexualorientation,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(sexualorientation,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
}
	if (filter == 'ethnicity'){
		filtered2<-subset(filtered,filtered$Z06 == 'bme')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$Z06 == 'Non BME')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		EI<-c(A[1],B[1])
		MyWork<-c(A[2],B[2])
		OrganisationalObjectives<-c(A[3],B[3])
		MyManager<-c(A[4],B[4])
		MyTeam<-c(A[5],B[5])
		LearningAndDevelopment<-c(A[6],B[6])
		InclusionAndFairTreatment<-c(A[7],B[7])
		ResourcesAndWorkload<-c(A[8],B[8])
		PayAndBenifits<-c(A[9],B[9])
		LeadershipAndManagingChange<-c(A[10],B[10])
		BullingAndHarasment<-c(A[11],B[11])
		N<-c(A[12],B[12])
		ethnicity<-c('bme','Non BME')
		dfei<-data.frame(ethnicity,EI,N)
		dfts<-data.frame(ethnicity,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(ethnicity,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
	if (filter == 'religion'){
		filtered2<-subset(filtered,filtered$J08 == 'Christian')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J08 == 'No religion')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J08 == 'Prefer not to say')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1])
		MyWork<-c(A[2],B[2],C[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3])
		MyManager<-c(A[4],B[4],C[4])
		MyTeam<-c(A[5],B[5],C[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8])
		PayAndBenifits<-c(A[9],B[9],C[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10])
		BullingAndHarasment<-c(A[11],B[11],C[11])
		N<-c(A[12],B[12],C[12])
		religion<-c('Christian','No religion','Prefer not to say')
		dfei<-data.frame(religion,EI,N)
		dfts<-data.frame(religion,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(religion,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
	if (filter == '(other care responsibility'){
		filtered2<-subset(filtered,filtered$J05 == 'Yes')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J05 == 'No')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J05 == 'Prefer not to say')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1])
		MyWork<-c(A[2],B[2],C[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3])
		MyManager<-c(A[4],B[4],C[4])
		MyTeam<-c(A[5],B[5],C[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8])
		PayAndBenifits<-c(A[9],B[9],C[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10])
		BullingAndHarasment<-c(A[11],B[11],C[11])
		N<-c(A[12],B[12],C[12])
		other.care.responsibility<-c('Yes','No','Prefer not to say')
		dfei<-data.fram(other.care.responsibility,EI,N)
		dfts<-data.frame(other.care.responsibility,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(other.care.responsibility,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
	if (filter == 'child care responsibility'){
		filtered2<-subset(filtered,filtered$J06 == 'Yes')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J06 == 'No')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$J06 == 'Prefer not to say')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1])
		MyWork<-c(A[2],B[2],C[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3])
		MyManager<-c(A[4],B[4],C[4])
		MyTeam<-c(A[5],B[5],C[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8])
		PayAndBenifits<-c(A[9],B[9],C[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10])
		BullingAndHarasment<-c(A[11],B[11],C[11])
		N<-c(A[12],B[12],C[12])
		child.care.responsibility<-c('Yes','No','Prefer not to say')
		dfei<-data.frame(child.care.responsibility,EI,N)
		dfts<-data.frame(child.care.responsibility,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(child.care.responsibility,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
	if (filter == 'working pattern'){
		filtered2<-subset(filtered,filtered$H06 == 'Full-time')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$H06 == 'Part-time')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$H06 == 'Job-share')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1])
		MyWork<-c(A[2],B[2],C[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3])
		MyManager<-c(A[4],B[4],C[4])
		MyTeam<-c(A[5],B[5],C[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8])
		PayAndBenifits<-c(A[9],B[9],C[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10])
		BullingAndHarasment<-c(A[11],B[11],C[11])
		N<-c(A[12],B[12],C[12])
		working.pattern<-c('Full-time','Part-time','Job-share')
		dfei<-data.frame(working.pattern,EI,N)
		dfts<-data.frame(working.pattern,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(working.pattern,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
		if (filter == 'location'){
		filtered2<-subset(filtered,filtered$location == 'London')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$location == 'Leeds')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$location == 'Other')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1])
		MyWork<-c(A[2],B[2],C[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3])
		MyManager<-c(A[4],B[4],C[4])
		MyTeam<-c(A[5],B[5],C[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8])
		PayAndBenifits<-c(A[9],B[9],C[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10])
		BullingAndHarasment<-c(A[11],B[11],C[11])
		N<-c(A[12],B[12],C[12])
		location<-c('London','Leeds','Other')
		dfei<-data.frame(location,EI,N)
		dfts<-data.frame(location,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(location,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
	if (filter == 'grade'){
		filtered2<-subset(filtered,filtered$K01_DH == 'ao')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$K01_DH == 'eo')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'heo')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)	
		filtered2<-subset(filtered,filtered$K01_DH == 'Civil Service Fast Stream')
		D<-c()
		for (x in 1:11){
			D[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		D[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'seo')
		E<-c()
		for (x in 1:11){
			E[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		E[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'Grade 7')
		F<-c()
		for (x in 1:11){
			F[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		F[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'Grade 6')
		G<-c()
		for (x in 1:11){
			G[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		G[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$K01_DH == 'Senior Civil Service')
		H<-c()
		for (x in 1:11){
			H[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		H[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1],D[1],E[1],F[1],G[1],H[1])
		MyWork<-c(A[2],B[2],C[2],D[2],E[2],F[2],G[2],H[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3],D[3],E[3],F[3],G[3],H[3])
		MyManager<-c(A[4],B[4],C[4],D[4],E[4],F[4],G[4],H[4])
		MyTeam<-c(A[5],B[5],C[5],D[5],E[5],F[5],G[5],H[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6],D[6],E[6],F[6],G[6],H[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7],D[7],E[7],F[7],G[7],H[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8],D[8],E[8],F[8],G[8],H[8])
		PayAndBenifits<-c(A[9],B[9],C[9],D[9],E[9],F[9],G[9],H[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10],D[10],E[10],F[10],G[10],H[10])
		BullingAndHarasment<-c(A[11],B[11],C[11],D[11],E[11],F[11],G[11],H[11])
		N<-c(A[12],B[12],C[12],D[12],E[12],F[12],G[12],H[12])
		grade<-c('AO','EO','HEO','Civil Service Fast Stream','SEO','Grade 7','Grade 6','Senior Civil Service')
		dfei<-data.frame(grade,EI,N)
		dfts<-data.frame(grade,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(grade,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
	if (filter == 'scs'){
		filtered2<-subset(filtered,filtered$Z03 == 'scs')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$Z03 == 'nonscs')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$Z03 == 'oth')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1])
		MyWork<-c(A[2],B[2],C[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3])
		MyManager<-c(A[4],B[4],C[4])
		MyTeam<-c(A[5],B[5],C[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8])
		PayAndBenifits<-c(A[9],B[9],C[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10])
		BullingAndHarasment<-c(A[11],B[11],C[11])
		N<-c(A[12],B[12],C[12])
		scs<-c('scs','non-scs','Other')
		dfei<-data.frame(scs,EI,N)
		dfts<-data.frame(scs,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(scs,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
		if (filter == 'time in role'){
		filtered2<-subset(filtered,filtered$H02 == 'Less than 6 months')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$H02 == 'At least 6 months but not more than 1 year')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$H02 == 'At least 1 year but not more than 3 years')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)	
		filtered2<-subset(filtered,filtered$H02 == 'At least 3 years but not more than 5 years')
		D<-c()
		for (x in 1:11){
			D[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		D[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$H02 == 'At least 5 years but not more than 10 years')
		E<-c()
		for (x in 1:11){
			E[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		E[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$H02 == 'At least 10 years but not more than 20 years')
		F<-c()
		for (x in 1:11){
			F[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		F[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$H02 == 'At least 20 years or more')
		G<-c()
		for (x in 1:11){
			G[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		G[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1],D[1],E[1],F[1],G[1])
		MyWork<-c(A[2],B[2],C[2],D[2],E[2],F[2],G[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3],D[3],E[3],F[3],G[3])
		MyManager<-c(A[4],B[4],C[4],D[4],E[4],F[4],G[4])
		MyTeam<-c(A[5],B[5],C[5],D[5],E[5],F[5],G[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6],D[6],E[6],F[6],G[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7],D[7],E[7],F[7],G[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8],D[8],E[8],F[8],G[8])
		PayAndBenifits<-c(A[9],B[9],C[9],D[9],E[9],F[9],G[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10],D[10],E[10],F[10],G[10])
		BullingAndHarasment<-c(A[11],B[11],C[11],D[11],E[11],F[11],G[11])
		N<-c(A[12],B[12],C[12],D[12],E[12],F[12],G[12])
		Time.in.role<-c('Less than 6 months','At least 6 months but not more than 1 year','At least 1 year but not more than 3 years','At least 3 years but not more than 5 years','At least 5 years but not more than 10 years','At least 10 years but not more than 20 years','At least 20 years or more')
		dfei<-data.frame(Time.in.role,EI,N)
		dfts<-data.frame(Time.in.role,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(Time.in.role,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
			if (filter == 'time in role'){
		filtered2<-subset(filtered,filtered$K01_DH == 'Less than 6 months')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 6 months but not more than 1 year')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 1 year but not more than 3 years')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)	
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 3 years but not more than 5 years')
		D<-c()
		for (x in 1:11){
			D[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		D[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 5 years but not more than 10 years')
		E<-c()
		for (x in 1:11){
			E[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		E[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 10 years but not more than 20 years')
		F<-c()
		for (x in 1:11){
			F[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		F[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 20 years or more')
		G<-c()
		for (x in 1:11){
			G[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		G[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1],D[1],E[1],F[1],G[1])
		MyWork<-c(A[2],B[2],C[2],D[2],E[2],F[2],G[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3],D[3],E[3],F[3],G[3])
		MyManager<-c(A[4],B[4],C[4],D[4],E[4],F[4],G[4])
		MyTeam<-c(A[5],B[5],C[5],D[5],E[5],F[5],G[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6],D[6],E[6],F[6],G[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7],D[7],E[7],F[7],G[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8],D[8],E[8],F[8],G[8])
		PayAndBenifits<-c(A[9],B[9],C[9],D[9],E[9],F[9],G[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10],D[10],E[10],F[10],G[10])
		BullingAndHarasment<-c(A[11],B[11],C[11],D[11],E[11],F[11],G[11])
		N<-c(A[12],B[12],C[12],D[12],E[12],F[12],G[12])
		Time.in.role<-c('Less than 6 months','At least 6 months but not more than 1 year','At least 1 year but not more than 3 years','At least 3 years but not more than 5 years','At least 5 years but not more than 10 years','At least 10 years but not more than 20 years','At least 20 years or more')
		dfei<-data.frame(Time.in.role,EI,N)
		dfts<-data.frame(Time.in.role,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(Time.in.role,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
			if (filter == 'time in role'){
		filtered2<-subset(filtered,filtered$K01_DH == 'Less than 6 months')
		A<-c()
		for (x in 1:11){
			A[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		A[12]<-nrow(filtered2)
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 6 months but not more than 1 year')
		B<-c()
		for (x in 1:11){
			B[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		B[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 1 year but not more than 3 years')
		C<-c()
		for (x in 1:11){
			C[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		C[12]<-nrow(filtered2)	
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 3 years but not more than 5 years')
		D<-c()
		for (x in 1:11){
			D[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		D[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 5 years but not more than 10 years')
		E<-c()
		for (x in 1:11){
			E[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		E[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 10 years but not more than 20 years')
		F<-c()
		for (x in 1:11){
			F[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		F[12]<-nrow(filtered2)		
		filtered2<-subset(filtered,filtered$K01_DH == 'At least 20 years or more')
		G<-c()
		for (x in 1:11){
			G[x]<-mean(filtered2[[variable[x]]], na.rm=TRUE)
		}
		G[12]<-nrow(filtered2)
		EI<-c(A[1],B[1],C[1],D[1],E[1],F[1],G[1])
		MyWork<-c(A[2],B[2],C[2],D[2],E[2],F[2],G[2])
		OrganisationalObjectives<-c(A[3],B[3],C[3],D[3],E[3],F[3],G[3])
		MyManager<-c(A[4],B[4],C[4],D[4],E[4],F[4],G[4])
		MyTeam<-c(A[5],B[5],C[5],D[5],E[5],F[5],G[5])
		LearningAndDevelopment<-c(A[6],B[6],C[6],D[6],E[6],F[6],G[6])
		InclusionAndFairTreatment<-c(A[7],B[7],C[7],D[7],E[7],F[7],G[7])
		ResourcesAndWorkload<-c(A[8],B[8],C[8],D[8],E[8],F[8],G[8])
		PayAndBenifits<-c(A[9],B[9],C[9],D[9],E[9],F[9],G[9])
		LeadershipAndManagingChange<-c(A[10],B[10],C[10],D[10],E[10],F[10],G[10])
		BullingAndHarasment<-c(A[11],B[11],C[11],D[11],E[11],F[11],G[11])
		N<-c(A[12],B[12],C[12],D[12],E[12],F[12],G[12])
		Time.in.role<-c('Less than 6 months','At least 6 months but not more than 1 year','At least 1 year but not more than 3 years','At least 3 years but not more than 5 years','At least 5 years but not more than 10 years','At least 10 years but not more than 20 years','At least 20 years or more')
		dfei<-data.frame(Time.in.role,EI,N)
		dfts<-data.frame(Time.in.role,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
		dfbh<-data.frame(Time.in.role,BullingAndHarasment,N)
		write.csv(dfei,paste('Engagement Index by',filter,'in',dir,'.csv'))
		write.csv(dfts,paste('Theme Score by',filter,'in',dir,'.csv'))
		write.csv(dfbh,paste('Bulling and Harasmant by',filter,'in',dir,'.csv'))
	}
}
EI<-c(GPH[1],FGO[1],CMO[1],CC[1],ACW[1],CSA[1],O[1])
MyWork<-c(GPH[2],FGO[2],CMO[2],CC[2],ACW[2],CSA[2],O[2])
OrganisationalObjectives<-c(GPH[3],FGO[3],CMO[3],CC[3],ACW[3],CSA[3],O[3])
MyManager<-c(GPH[4],FGO[4],CMO[4],CC[4],ACW[4],CSA[4],O[4])
MyTeam<-c(GPH[5],FGO[5],CMO[5],CC[5],ACW[5],CSA[5],O[5])
LearningAndDevelopment<-c(GPH[6],FGO[6],CMO[6],CC[6],ACW[6],CSA[6],O[6])
InclusionAndFairTreatment<-c(GPH[7],FGO[7],CMO[7],CC[7],ACW[7],CSA[7],O[7])
ResourcesAndWorkload<-c(GPH[8],FGO[8],CMO[8],CC[8],ACW[8],CSA[8],O[8])
PayAndBenifits<-c(GPH[9],FGO[9],CMO[9],CC[9],ACW[9],CSA[9],O[9])
LeadershipAndManagingChange<-c(GPH[10],FGO[10],CMO[10],CC[10],ACW[10],CSA[10],O[10])
BullingAndHarasment<-c(GPH[11],FGO[11],CMO[11],CC[11],ACW[11],CSA[11],O[11])
N<-c(GPH[12],FGO[12],CMO[12],CC[12],ACW[12],CSA[12],O[12])
direc<-c('Global & Public Health','Finance & Group Operations','Chief Medical Officer','Community Care','Acute Care & Workforce','Chief Scientific Adviser','Other')
dfei<-data.frame(direc,EI,N)
dfts<-data.frame(direc,MyWork,OrganisationalObjectives,MyManager,MyTeam,LearningAndDevelopment,InclusionAndFairTreatment,ResourcesAndWorkload,PayAndBenifits,LeadershipAndManagingChange,N)
dfbh<-data.frame(direc,BullingAndHarasment,N)
write.csv(dfei,'Engagement Index in directorates.csv')
write.csv(dfts,'Theme Score in directorates.csv')
write.csv(dfbh,'Bulling and Harasment in directorates.csv')