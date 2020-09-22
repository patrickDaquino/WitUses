<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, 7.6 of lundi 3 mars 2008 on mardi 22 septembre 2020 at 7:38:54</time-stamp>
<!-- Package WitUses* -->


<component-property>
<name>WitUses</name> <type>package</type>
<property>packageName</property> <value>'WitUses'</value>
</component-property>

<component-property>
<name>WitUses</name> <type>package</type>
<property>parcelName</property> <value>'WitUses'</value>
</component-property>

<name-space>
<name>WitUses</name>
<environment>CormasNS.Models</environment>
<private>false</private>
<imports>
			private Smalltalk.*
			private CormasNS.Kernel.*
			</imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</name-space>

<class>
<name>RulesImplementer</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.Agent</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>mySeasonPlotUses myRessProviders myUsers closureDuration hotResource closingSeasons coveringHoldedPlots riskyplots priorityUsers landTenureRule landUsesRule envRule socialRule sahelianRule backUpOfAllSeasonPlotUses backUpAppliedRules backUpMyTerritory mySeasonRessProviderUses </inst-vars>
<class-inst-vars>closureRate minPlotRessCoeff myTerritory </class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>SouthWest</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.RulesImplementer</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>SeasonRessProviderUse</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.PassiveObject</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>hostedPlot use season currentUsers </inst-vars>
<class-inst-vars>pastUsers </class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>Plot</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.SpatialEntityElement</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>landscape remainRessRS remainRessSr remainRess_r remainRess_s remainRessRs initRessRS initRessRs initRessSr initRess_r initRess_s overUseRs overUseSr potentialRessRS potentialRessRs potentialRessSr potentialRess_r potentialRess_s overUse_r memoOverUse_r memoOverUseSr memoOverUseRs holder closureDuration zoning </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>Use</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.PassiveObject</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars>requiredCapital requiredInvest ressTapping incomesRate needRS overYearlyThresholdRess resilienceRatioRess overRessUseRatio myRadius </class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>NorthEast</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.RulesImplementer</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>WitUses</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.CormasModel</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>thePlots numOfUsers theUsers theEnVariabilitys theSeasonPlotUses theRessProviders numOfSetlledUses numOfMobileUses numOfPickUses numOfIntensiveUses theCalibrations numOfSimulationParameterss theRulesImplementers theSeasonRessProviderUses theUses theSettledUses theMobileUses thePickUses theIntensiveUses numOfMonitorings numOfGlobals theNorthWests numOfNorthWests theSouthWests numOfSouthWests theNorthEasts numOfNorthEasts theSouthEasts numOfSouthEasts theOveralls numOfOveralls thePolicyMakers numOfPolicyMakers </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>User</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.GroupCommLocation</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>socialGroup capital allRessProviders myTerritory myLivestock allSeasonPlotUses myCurrentSPUs myHungryLivestock yearlyTappedRessRs seasonTappedRess_r seasonTappedRessSr seasonTappedRessRS_Settled seasonTappedRessRS_Mobile seasonTappedRessRs allUsers appliedRules allRulesImplementers hotPlots yearlyStartingPlot </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>MobileUse</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.Use</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars>coeffMinYearlyTappRess </class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>PolicyMaker</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.Agent</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>allRulesImplementers overallLandTenure overallLandUses overallEnv overallSocial overallSahelian nWLandTenure nWLandUses nWEnv nWSocial nWSahelian nELandTenure nELandUses nEEnv nESocial nESahelian sWLandTenure sWLandUses sWEnv sWSocial sWSahelian sELandTenure sELandUses sEEnv sESocial sESahelian allPlots allSeasonPlotUses allUsers </inst-vars>
<class-inst-vars>policyDuration </class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>NorthWest</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.RulesImplementer</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>EnVariability</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.PassiveObject</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>myTerritory myRessProviderUses landscape_I_Ratio landscape_II_Ratio landscape_III_Ratio landscape_IV_Ratio myRessProviders mySeasonPlotUses </inst-vars>
<class-inst-vars>nberSteadyRessProv nberTempRessProv usersNber </class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>IntensiveUse</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.Use</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>Overall</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.RulesImplementer</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>subRulesImplementers </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>SouthEast</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.RulesImplementer</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>RessProvider</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.ObjectLocation</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>ressType remainRess providerType initRess potentialRess </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>SeasonPlotUse</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.PassiveObject</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>pastUsers currentUsers hostedPlot season use </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>PickUse</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.Use</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>Calibration</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Kernel.PassiveObject</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars>ressCoeff useCoeffRSTemp userInitCapital livingStandard ressOfTempRessProv ressOfSteadyRessProv landscapeIIIRessValue landscapeIIRessValue landscapeIRessValue landscapeIVRessValue </class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>

<class>
<name>SettledUse</name>
<environment>CormasNS.Models.WitUses</environment>
<super>CormasNS.Models.WitUses.Use</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>WitUsesCategory</category>
<attributes>
<package>WitUses</package>
</attributes>
</class>






















<methods>
<class-id>CormasNS.Models.WitUses.SeasonPlotUse class</class-id> <category>+ parameter unit</category>

<body package="WitUses">pastUsers_type 
	"Return the type of attribute #pastUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">use_type 
	"Return the type of attribute #use.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Array</body>

<body package="WitUses">oldestUsers_type 
	"Return the type of attribute #oldestUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">currentUsers_type 
	"Return the type of attribute #currentUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^IndexedSet</body>

<body package="WitUses">hostedPlot_type 
	"Return the type of attribute #hostedPlot.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SeasonPlotUse class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SeasonPlotUse class</class-id> <category>- default value</category>

<body package="WitUses">hostedPlot_default 
	"Return the default value of attribute #hostedPlot : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">season_default 
	"Return the default value of attribute #season : #(1 2 3 4).	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^#(1 2 3 4)</body>

<body package="WitUses">use_default 
	"Return the default value of attribute #use : #('Picking' 'Settled' 'Mobile' 'Intensive').	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^#('Picking' 'Settled' 'Mobile' 'Intensive')</body>

<body package="WitUses">currentUsers_default 
	"Return the default value of attribute #currentUsers : IndexedSet new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^IndexedSet new</body>

<body package="WitUses">pastUsers_default 
	"Return the default value of attribute #pastUsers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">oldestUsers_default 
	"Return the default value of attribute #oldestUsers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SeasonPlotUse</class-id> <category>control</category>

<body package="WitUses">userMemoryPastUsersUpdate

"On repart sans modifier currentUsers, mais en ayant conservé la mémoire des pastUsers"

self pastUsers addAll: self currentUsers.</body>

<body package="WitUses">userMemoryUpdate

" en début d'année, on repart sans currentUsers, mais en ayant conservé la mémoire de quels usages on a effectués"

self pastUsers addAll: self currentUsers. self currentUsers removeAll.</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SeasonPlotUse</class-id> <category>accessing</category>

<body package="WitUses">oldestUsers: aOrderedCollection 
	"Setter accessor of attribute oldestUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	pastUsers := aOrderedCollection</body>

<body package="WitUses">oldestUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^pastUsers ifNil:[pastUsers := OrderedCollection new]</body>

<body package="WitUses">season
	"Getter accessor with initial value = #(1 2 3 4). 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^season ifNil:[season := #(1 2 3 4)]</body>

<body package="WitUses">use
	"Getter accessor with initial value = #('Picking' 'Settled' 'Mobile' 'Intensive'). 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := #('Picking' 'Settled' 'Mobile' 'Intensive')]</body>

<body package="WitUses">activity: aArray 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use := aArray</body>

<body package="WitUses">season: aArray 
	"Setter accessor of attribute season	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	season := aArray</body>

<body package="WitUses">activity
	"Getter accessor with initial value = #('Settled' 'Mobile' 'Pick' 'Intensive'). 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := #('Settled' 'Mobile' 'Pick' 'Intensive')]</body>

<body package="WitUses">currentUsers
	"Getter accessor with initial value = IndexedSet new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^currentUsers ifNil:[currentUsers := IndexedSet new]</body>

<body package="WitUses">hostedPlot
	"Getter accessor without initial value "
	^hostedPlot</body>

<body package="WitUses">hostedPlot: aUndefinedObject 
	"Setter accessor of attribute hostedPlot	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	hostedPlot := aUndefinedObject</body>

<body package="WitUses">pastUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^pastUsers ifNil:[pastUsers := OrderedCollection new]</body>

<body package="WitUses">pastUsers: aOrderedCollection 
	"Setter accessor of attribute pastUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	pastUsers := aOrderedCollection</body>

<body package="WitUses">currentUsers: aIndexedSet 
	"Setter accessor of attribute currentUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	currentUsers := aIndexedSet</body>

<body package="WitUses">use: aArray 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use := aArray</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer class</class-id> <category>+ parameter unit</category>

<body package="WitUses">priorityPrinciple_type 
	"Return the type of attribute #priorityPrinciple.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Array</body>

<body package="WitUses">mySeasonsRessProviderUses_type 
	"Return the type of attribute #mySeasonsRessProviderUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">closureDuration_type 
	"Return the type of attribute #closureDuration.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landUsesPolicy_type 
	"Return the type of attribute #landUsesPolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">backUpMyTerritory_type 
	"Return the type of attribute #backUpMyTerritory.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">priorityUsers_type 
	"Return the type of attribute #priorityUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">backUpEnvPolicy_type 
	"Return the type of attribute #backUpEnvPolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">landTenurePolicy_type 
	"Return the type of attribute #landTenurePolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">initTerritoryRess_unit 
	"Return the unit of attribute #initTerritoryRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">northeast_type 
	"Return the type of attribute #northeast.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">minPlotRessCoeff_unit 
	"Return the unit of attribute #minPlotRessCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">riskyPlots_type 
	"Return the type of attribute #riskyPlots.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">localPolicy_type 
	"Return the type of attribute #localPolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">initTerritoryRess_type 
	"Return the type of attribute #initTerritoryRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">socialPolicy_type 
	"Return the type of attribute #socialPolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">myTerritory_type 
	"Return the type of attribute #myTerritory.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">hotResource_type 
	"Return the type of attribute #hotResource.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">myUsers_type 
	"Return the type of attribute #myUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">closureRate_unit 
	"Return the unit of attribute #closureRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">closureRate_type 
	"Return the type of attribute #closureRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">closingSeasons_type 
	"Return the type of attribute #closingSeasons.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Array</body>

<body package="WitUses">minPlotRessCoeff_type 
	"Return the type of attribute #minPlotRessCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">coveringHoldedPlots_type 
	"Return the type of attribute #coveringHoldedPlots.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">backUpOfAllSeasonPlotUses_type 
	"Return the type of attribute #backUpOfAllSeasonPlotUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">backUpAppliedPolicies_type 
	"Return the type of attribute #backUpAppliedPolicies.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">myPlotsAllocations_type 
	"Return the type of attribute #myPlotsAllocations.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">envPolicy_type 
	"Return the type of attribute #envPolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">myRessProvidAllocations_type 
	"Return the type of attribute #myRessProvidAllocations.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">closureDuration_unit 
	"Return the unit of attribute #closureDuration.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">policy_type 
	"Return the type of attribute #policy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer class</class-id> <category>accessing</category>

<body package="WitUses">myTerritory: aOrderedCollection 
	"Setter accessor of attribute myTerritory	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myTerritory = aOrderedCollection ifTrue:[^nil].   "for optimization"
	myTerritory := aOrderedCollection. 
	self changed</body>

<body package="WitUses">closureRate: aFloat 
	"Setter accessor of attribute closureRate	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	closureRate = aFloat ifTrue:[^nil].   "for optimization"
	closureRate := aFloat. 
	self changed</body>

<body package="WitUses">minPlotRessCoeff: aFloat 
	"Setter accessor of attribute minPlotRessCoeff	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	minPlotRessCoeff = aFloat ifTrue:[^nil].   "for optimization"
	minPlotRessCoeff := aFloat. 
	self changed</body>

<body package="WitUses">minPlotRessCoeff
	"Getter accessor with initial value = 0.9. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^minPlotRessCoeff ifNil:[minPlotRessCoeff := 0.9]</body>

<body package="WitUses">myTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myTerritory ifNil:[myTerritory := OrderedCollection new]</body>

<body package="WitUses">closureRate
	"Getter accessor with initial value = 0.125. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^closureRate ifNil:[closureRate := 0.125]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^true</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer class</class-id> <category>- default value</category>

<body package="WitUses">myTerritory_default 
	"Return the default value of attribute #myTerritory : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">coveringHoldedPlots_default 
	"Return the default value of attribute #coveringHoldedPlots : 'Yes'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'Yes'</body>

<body package="WitUses">mySeasonsRessProviderUses_default 
	"Return the default value of attribute #mySeasonsRessProviderUses : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">myPlotsAllocations_default 
	"Return the default value of attribute #myPlotsAllocations : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">landUsesPolicy_default 
	"Return the default value of attribute #landUsesPolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">socialRule_default 
	"Return the default value of attribute #socialRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">policy_default 
	"Return the default value of attribute #policy : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">hotResource_default 
	"Return the default value of attribute #hotResource : 'Sr'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'Sr'</body>

<body package="WitUses">priorityUsers_default 
	"Return the default value of attribute #priorityUsers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">sahelianPolicy_default 
	"Return the default value of attribute #sahelianPolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">closureRate_default 
	"Return the default value of attribute #closureRate : 0.125.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.125</body>

<body package="WitUses">backUpAppliedPolicies_default 
	"Return the default value of attribute #backUpAppliedPolicies : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">landTenurePolicy_default 
	"Return the default value of attribute #landTenurePolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sahelianRule_default 
	"Return the default value of attribute #sahelianRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">closureDuration_default 
	"Return the default value of attribute #closureDuration : 3.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^3</body>

<body package="WitUses">northeast_default 
	"Return the default value of attribute #northeast : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">landUsesRule_default 
	"Return the default value of attribute #landUsesRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">envRule_default 
	"Return the default value of attribute #envRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">landTenureRule_default 
	"Return the default value of attribute #landTenureRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">myUsers_default 
	"Return the default value of attribute #myUsers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">riskyPlots_default 
	"Return the default value of attribute #riskyPlots : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">initTerritoryRess_default 
	"Return the default value of attribute #initTerritoryRess : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">priorityPrinciple_default 
	"Return the default value of attribute #priorityPrinciple : #('PlotResources' 'TerritoryResources' 'Households').	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^#('PlotResources' 'TerritoryResources' 'Households')</body>

<body package="WitUses">localPolicy_default 
	"Return the default value of attribute #localPolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">backUpOfAllSeasonPlotUses_default 
	"Return the default value of attribute #backUpOfAllSeasonPlotUses : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">closingSeasons_default 
	"Return the default value of attribute #closingSeasons : #(3 4).	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^#(3 4)</body>

<body package="WitUses">envPolicy_default 
	"Return the default value of attribute #envPolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">backUpEnvPolicy_default 
	"Return the default value of attribute #backUpEnvPolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">socialPolicy_default 
	"Return the default value of attribute #socialPolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">minPlotRessCoeff_default 
	"Return the default value of attribute #minPlotRessCoeff : 0.9.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.9</body>

<body package="WitUses">myRessProvidAllocations_default 
	"Return the default value of attribute #myRessProvidAllocations : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">backUpMyTerritory_default 
	"Return the default value of attribute #backUpMyTerritory : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>accessing</category>

<body package="WitUses">sahelianRule: aByteString 
	"Setter accessor of attribute localPolicy	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sahelianRule := aByteString</body>

<body package="WitUses">closureDuration: aSmallInteger 
	"Setter accessor of attribute closureDuration	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	closureDuration = aSmallInteger ifTrue:[^nil].   "for optimization"
	closureDuration := aSmallInteger. 
	self changed</body>

<body package="WitUses">myRessProviders
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myRessProviders ifNil:[myRessProviders := OrderedCollection new]</body>

<body package="WitUses">mySeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonPlotUses ifNil:[mySeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">mySeasonRessProviderUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonRessProviderUses ifNil:[mySeasonRessProviderUses := OrderedCollection new]</body>

<body package="WitUses">closingSeasons
	"Getter accessor with initial value = #(3 4). 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^closingSeasons ifNil:[closingSeasons := #(3 4)]</body>

<body package="WitUses">envRule: aByteString 
	"Setter accessor of attribute envPolicy	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	envRule = aByteString ifTrue:[^nil].   "for optimization"
	envRule := aByteString. 
	self changed</body>

<body package="WitUses">backUpMyTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpMyTerritory ifNil:[backUpMyTerritory := OrderedCollection new]</body>

<body package="WitUses">mySeasonPlotUses: aOrderedCollection 
	"Setter accessor of attribute myPlotsAllocations	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	mySeasonPlotUses := aOrderedCollection</body>

<body package="WitUses">sahelianRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sahelianRule ifNil:[sahelianRule := '']</body>

<body package="WitUses">hotResource
	"Getter accessor with initial value = 'Sr'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^hotResource ifNil:[hotResource := 'Sr']</body>

<body package="WitUses">closureDuration
	"Getter accessor with initial value = 3. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^closureDuration ifNil:[closureDuration := 3]</body>

<body package="WitUses">backUpMyTerritory: aOrderedCollection 
	"Setter accessor of attribute backUpMyTerritory	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	backUpMyTerritory = aOrderedCollection ifTrue:[^nil].   "for optimization"
	backUpMyTerritory := aOrderedCollection. 
	self changed</body>

<body package="WitUses">myUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myUsers ifNil:[myUsers := OrderedCollection new]</body>

<body package="WitUses">backUpOfAllSeasonPlotUses: aOrderedCollection 
	"Setter accessor of attribute backUpOfAllSeasonPlotUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	backUpOfAllSeasonPlotUses = aOrderedCollection ifTrue:[^nil].   "for optimization"
	backUpOfAllSeasonPlotUses := aOrderedCollection. 
	self changed</body>

<body package="WitUses">envRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^envRule ifNil:[envRule := '']</body>

<body package="WitUses">backUpAppliedRules
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpAppliedRules ifNil:[backUpAppliedRules := OrderedCollection new]</body>

<body package="WitUses">landTenureRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landTenureRule ifNil:[landTenureRule := '']</body>

<body package="WitUses">riskyplots
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^riskyplots ifNil:[riskyplots := OrderedCollection new]</body>

<body package="WitUses">socialRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^socialRule ifNil:[socialRule := '']</body>

<body package="WitUses">backUpOfAllSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpOfAllSeasonPlotUses ifNil:[backUpOfAllSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">hotResource: aByteString 
	"Setter accessor of attribute hotResource	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	hotResource := aByteString</body>

<body package="WitUses">priorityUsers: aOrderedCollection 
	"Setter accessor of attribute priorityUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	priorityUsers = aOrderedCollection ifTrue:[^nil].   "for optimization"
	priorityUsers := aOrderedCollection. 
	self changed</body>

<body package="WitUses">riskyplots: aOrderedCollection 
	"Setter accessor of attribute riskyPlots	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	riskyplots := aOrderedCollection</body>

<body package="WitUses">coveringHoldedPlots
	"Getter accessor with initial value = 'Yes'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^coveringHoldedPlots ifNil:[coveringHoldedPlots := 'Yes']</body>

<body package="WitUses">coveringHoldedPlots: aByteString 
	"Setter accessor of attribute coveringHoldedPlots	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	coveringHoldedPlots := aByteString</body>

<body package="WitUses">myUsers: aOrderedCollection 
	"Setter accessor of attribute myUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myUsers := aOrderedCollection</body>

<body package="WitUses">landUsesRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landUsesRule ifNil:[landUsesRule := '']</body>

<body package="WitUses">priorityUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^priorityUsers ifNil:[priorityUsers := OrderedCollection new]</body>

<body package="WitUses">socialRule: aByteString 
	"Setter accessor of attribute socialPolicy	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	socialRule = aByteString ifTrue:[^nil].   "for optimization"
	socialRule := aByteString. 
	self changed</body>

<body package="WitUses">backUpAppliedRules: aOrderedCollection 
	"Setter accessor of attribute backUpAppliedPolicies	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	backUpAppliedRules = aOrderedCollection ifTrue:[^nil].   "for optimization"
	backUpAppliedRules := aOrderedCollection. 
	self changed</body>

<body package="WitUses">landTenureRule: aByteString 
	"Setter accessor of attribute landTenurePolicy	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landTenureRule = aByteString ifTrue:[^nil].   "for optimization"
	landTenureRule := aByteString. 
	self changed</body>

<body package="WitUses">landUsesRule: aByteString 
	"Setter accessor of attribute landUsesPolicy	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landUsesRule = aByteString ifTrue:[^nil].   "for optimization"
	landUsesRule := aByteString. 
	self changed</body>

<body package="WitUses">myRessProviders: aOrderedCollection 
	"Setter accessor of attribute myRessProvidAllocations	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myRessProviders := aOrderedCollection</body>

<body package="WitUses">closingSeasons: aArray 
	"Setter accessor of attribute closingSeasons	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	closingSeasons := aArray</body>

<body package="WitUses">mySeasonRessProviderUses: aOrderedCollection 
	"Setter accessor of attribute mySeasonsRessProviderUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	mySeasonRessProviderUses = aOrderedCollection ifTrue:[^nil].   "for optimization"
	mySeasonRessProviderUses := aOrderedCollection. 
	self changed</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>sahelianRules</category>

<body package="WitUses">sociaNetwork

" S'il n'y a pas de spus libres adéquats pour un user dans son voisinage (donc APRES EXPLORENEIGHBOUR), il peut accéder dans tout le territoire aux spus des membres de son groupe social (l'usager inclus dans ses droits affectés ceux de tous les autres membres de son groupe social)"

self myUsers do: [:u | u applied policies add: 'socialNetwork'].</body>

<body package="WitUses">selectingHotPlots

| sortedPlots rate hotPlots |

"On repère les plots aux ressources clés"
hotPlots:= IndexedSet new. sortedPlots:= IndexedSet new.
"je classe les plots en fonction de leur quantité de ressources, et je récupère les plus riches"
sortedPlots:= self class myTerritory asSortedCollection: [:plot1 :plot2 | (plot1 initRessSr + plot1 initRess_r + plot1 initRessRs) &lt; (plot2 initRessSr + plot2 initRess_r + plot2 initRessRs)].
rate:= (self class closureRate * 144) asInteger. sortedPlots size - rate to: sortedPlots size do: [:plot | hotPlots add: (sortedPlots at: plot)].
"et je visualise les hotsPlots via l'attribut zoning"
self class myTerritory do: [:p | (hotPlots includes:p) ifTrue: [p zoning: 'hotPlot']].
"et on communique aux users ces hotPlots"
self sendingToUser:hotPlots.</body>

<body package="WitUses">identifyingWeatherCrisis

| initTerrRSTot potentialTerrRSTot |

"On repère si crise climatique de façon indirecte, par la quantité d'eau (ress RS) disponible dans les plots au moment des pluies (début saison 1)"
initTerrRSTot:=0. potentialTerrRSTot:=0.
self class myTerritory do: [:p | initTerrRSTot:= initTerrRSTot + p initRessRS. potentialTerrRSTot:= potentialTerrRSTot + p potentialRessRS].
(initTerrRSTot &lt; (self class minPlotRessCoeff * potentialTerrRSTot))
ifTrue: [
"et si crise, on sélectionne alors les plots aux ressources clés"
self selectingHotPlots].</body>

<body package="WitUses">weatherCrisis


"On repère si crise climatique de façon indirecte, par la quantité d'eau (ress RS) disponible dans les plots AU MOMENT SAISON DES PLUIES (début saison 1)"
((WitUses timeStep\\4)=1) ifTrue:[
(PolicyMaker policyDuration isNil) ifTrue: [self identifyingWeatherCrisis]
"...si une policy est appliquée, selon le rythme de renouvellement de la policy"
ifFalse: [(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:[self identifyingWeatherCrisis]]].</body>

<body package="WitUses">ressCrisis

"Repère tous les plots qui ont une de leurs ressources trop dégradée et en met une fraction en défens..
Au début de chaque année, ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total d'une ressource d'un plot (attribut self hotRessource), hors eau du sol, et pour l'instant _s, est inférieur à un minimum (attribut self minPlotRessCoeff), alors les usages utilisant cette ressource (et les usages dégradant cette ressource) sont interdits sur une fraction (attribut self closureRate) des plots dont la quantité de cette ressource est la plus faible, pour x années (attribut self closureDuration) et pour les saisons sélectionnées (attribut self closingResources)"

"On repère d'abord si crise en ressources"
(PolicyMaker policyDuration isNil) ifTrue: [self identifyingRessCrisis]
"...si une policy est appliquée, selon le rythme de renouvellement de la policy"
ifFalse: [(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:[self identifyingRessCrisis]].</body>

<body package="WitUses">sendingToUser:hotPlots

self myUsers do: [:u | u hotPlots addAll: hotPlots].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>control</category>

<body package="WitUses">openingUsing
	"Les users se lancent dans les activités, en fonction de l'ordre de priorité permis par la policy (le Cormas mix est trop long pour cet ordi)"

	| firstUsers otherUsers |

(self priorityUsers isNotEmpty)
ifTrue: [firstUsers := self myUsers select: [:u | self priorityUsers includes: u]. otherUsers:= self myUsers copy. otherUsers removeAll: firstUsers.
firstUsers , "(Cormas mix: otherUsers)" otherUsers do: [:u | u implementUses]]
ifFalse: ["(Cormas mix: self myUsers)" self myUsers do: [: u | u implementUses]].</body>

<body package="WitUses">visualizingEnvRule
	"comment stating purpose of message"

| mySPUS |

mySPUS:= self mySeasonPlotUses select: [:spu | spu currentUsers includes: self].
(mySPUS collect: [:spu | spu hostedPlot]) do: [:p | p holder: self].</body>

<body package="WitUses">applyingRules

"landTenureRules (only for Overall RulesImplementer): firstUser', onePlotOneUser, lastUser, socialNetwork
envRules: plotRessClosing, plotTotClosing, terrTotClosing, terrRessClosing, livingStandard, weatherCrisisPolicy, ressCrisisPolicy
landUsesRules : zoning, localist
localRules: weatherCrisis, ressCrisis
socialRules (only for Overall RulesImplementer): livingStandard"
self myUsers do: [:u | u appliedRules add: self landTenureRule , self landUsesRule, self envRule, self socialRule, self socialRule].
(self landTenureRule isEmpty) ifFalse: [self perform: self landTenureRule asSymbol].
(self landUsesRule isEmpty) ifFalse: [self perform: self landUsesRule asSymbol].
(self envRule isEmpty) ifFalse: [self perform: self envRule asSymbol].
(self socialRule isEmpty) ifFalse: [self perform: self socialRule asSymbol].
(self sahelianRule isEmpty) ifFalse: [self perform: self sahelianRule asSymbol].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>landTenureRules</category>

<body package="WitUses">onePlotOneUser

| theWinner theWinnerPlot | 

"Affectation permanente de tous les SPU d'un plot (quel que soit l'usage ou la saison) à un seul user"

self mySeasonPlotUses do: [:spu |
"si le plot n'est pas en défens..."
(spu currentUsers includes:self) ifFalse:[
"l'heureux affectataire est le premier des currentUsers, ou des pastUsers"
(spu currentUsers isEmpty)
ifFalse: [theWinner:= spu currentUsers first]
ifTrue: [(spu pastUsers isNotEmpty) ifTrue: [theWinner:= spu pastUsers first. spu currentUsers add: theWinner]]]].
"attribution de tous les spus du plot à l'affectataire"
self mySeasonPlotUses do: [:spu | (spu currentUsers includes:theWinner) ifTrue: [theWinnerPlot:=spu hostedPlot. (spu hostedPlot=theWinnerPlot) ifTrue: [spu currentUsers removeAll; add:theWinner]]].
"inscription d'un indicateur spatial repérant l'attribution du plot"
(self class myTerritory select: [:p | p=theWinnerPlot]) do: [:p | p holder: theWinner].</body>

<body package="WitUses">lastUser

"Affectation à chaque saison du SPU concerné au dernier qui l'a déjà utilisé"
self mySeasonPlotUses do: [:spu | (spu pastUsers isNotEmpty) ifTrue: [spu currentUsers add: spu pastUsers last]].</body>

<body package="WitUses">firstUser

"Affectation à chaque saison du SPU concerné au premier user qui l'a déjà utilisé"
self mySeasonPlotUses do: [:spu | (spu currentUsers includes: self) ifFalse: [(spu pastUsers isEmpty) ifFalse: [spu currentUsers add: spu pastUsers first]]].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>landUsesRules</category>

<body package="WitUses">zoning
	"DECOUPAGE MANUEL du territoire (A REALISER APRES INITIALISATION) en zones exclusivement réservées à certains usages (type plan d'occupation des sols). Pas de méthode spécifique pour spécifier les zones, se fait directement en renseignant manuellement l'attribut 'zoning' des cellules spatiales, sur l'interface de visualisation spatiale. Ce type de politique étant moins contraignante que les politiques foncières et environnementales, elle est implémentée avant les méthodes de ces politiques, qui peuvent ainsi ne pas la respecter"

"affecte les droits tels qu'inscrit dans l'attribut zoning des plots"
self class myTerritory do: [:p |
(p zoning = 'Settled') ifTrue: [(self mySeasonPlotUses select: [:spu | (spu hostedPlot = p)]) do: [:spu | (spu use = 'Settled') ifFalse: [spu currentUsers add: self]]].
(p zoning = 'MobilePicking') ifTrue: [(self mySeasonPlotUses select: [:spu | (spu hostedPlot = p)]) do: [:spu | (spu use ~= 'Settled') ifTrue: [spu currentUsers add: self]]].
(p zoning = 'Picking') ifTrue: [(self mySeasonPlotUses select: [:spu | (spu hostedPlot = p)]) do: [:spu | (spu use = 'Picking') ifFalse: [spu currentUsers add: self]]]
					]</body>

<body package="WitUses">oneTerritoryLocalist

"accès du territoire du RulesImplementer réservé aux users qui s'y trouvaient à l'initialisation (autrement dit, liaison sous-territoires réservés aux users y 'habitant'). Si c'est overall, la règle localiste est appliquée par chacun des sub-territoires"

| myTerritorializedUsers |
 
"je repère les users présents sur mon territoire"
(WitUses timeStep = 1) ifTrue:[
myTerritorializedUsers:= (self myUsers select: [:u | self class myTerritory contains: [:p | p =u patch]]).
"si ce n'est pas une politique overall, j'exclue mon territoire, et les spu correspondants, de tous les usagers (si cela en est une, pas besoin de le faire, chaque sous territoire installant son propre territoire)"
(self class = Overall) ifFalse: [self myUsers do: [:u | u myTerritory removeAllSuchThat: [:p | self class myTerritory includes: p]. u allSeasonPlotUses: (u allSeasonPlotUses select: [:spu | u myTerritory contains: [:p | p= spu hostedPlot]])]].
"puis je récupère les usagers présents sur mon territoire comme mes seuls usagers"
self myUsers: myTerritorializedUsers.
"et je restreins leur territoire et leurs droits potentiels d'usage à mon propre territoire, tandis que j'enlève mon territoire des autres usagers"
self myUsers do: [:u | u myTerritory: self class myTerritory. u allSeasonPlotUses: self mySeasonPlotUses]]</body>

<body package="WitUses">wholeTerritoryLocalist

"accès du territoire du RulesImplementer réservé aux users qui s'y trouvaient à l'initialisation (autrement dit, liaison sous-territoires réservés aux users y 'habitant'). Si c'est overall, la règle localiste est appliquée par chacun des sub-territoires"

"(self class = Overall) ifFalse: [self oneTerritoryLocalist] ifTrue: [self subRulesImplementers do: [:r | r oneTerritoryLocalist]]."
| myTerritorializedUsers |
 
"chacun des sub RulesImplementers repère les users présents sur mon territoire"
(WitUses timeStep = 1) ifTrue:[
self subRulesImplementers do: [:sri | 
myTerritorializedUsers:= (sri myUsers select: [:u | sri class myTerritory contains: [:p | p =u patch]]).
"puis récupère les usagers présents sur leur territoire comme ses seuls usagers"
sri myUsers: myTerritorializedUsers.
"et restreint leur territoire et leurs droits potentiels d'usage à son unique territoire"
sri myUsers do: [:u | u myTerritory: sri class myTerritory. u allSeasonPlotUses: sri mySeasonPlotUses]]]</body>

<body package="WitUses">localist

"accès du territoire du RulesImplementer réservé aux users qui s'y trouvaient à l'initialisation (autrement dit, liaison sous-territoires réservés aux users y 'habitant'). Si c'est overall, la règle localiste est appliquée par chacun des sub-territoires"

(self class = Overall) ifFalse: [self oneTerritoryLocalist] ifTrue: [self wholeTerritoryLocalist].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>envRules</category>

<body package="WitUses">selectTerrRessClosing

"Vison globale des quantités totales de chaque ressource au niveau territorial (pas un indicateur par plot).
Au début de chaque année, ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total d'une ressource du territoire (hors eau du sol, et pour l'instant _s) est inférieur à un minimum (attribut self minPlotRessCoeff), alors l'usage de cette ressource (et les usages dégradant cette ressource) sont interdits sur une fraction (attribut self closureRate) des plots les moins fournis concernant cette ressource, aux saisons sélectionnées (attribut self closingResources) et pour x années (attribut self closureDuration)"

| riskyPlots |

riskyPlots:= OrderedCollection new.
"puis au sujet de la ressource sélectionnée (self hotRessource) les plots ayant un trop grand écart (self minPlotRessCoeff) entre leur total potentiel et réel sont repérés"
self assessAndSelectTerrRessConditions: riskyPlots.
"et les usages concernant cette ressource sont interdits sur une fraction des plots concernés"
(riskyPlots isNotEmpty) ifTrue: [self protectTerrRess: riskyPlots.
"inscription d'un indicateur spatial repérant l'attribution du plot" self visualizingEnvRule]</body>

<body package="WitUses">selectTerrTotClosing

"Vision globale de la quantité totale de ressources au niveau territorial (pas un indicateur par plot).
Au début de chaque année ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total des ressources du territoire (hors eau du sol, et pour l'instant _s) est inférieur à un minimum (attribut self minPlotRessCoeff), alors sont mis en défens pour x années (attribut self closureDuration) et sur les saisons sélectionnées (attribut self closingSeasons) une fraction (attribut self closureRate) des plots les moins fournis en ressources"

| riskyPlots |

riskyPlots:= OrderedCollection new.
"Si l'écart (self minPlotRessCoeff) entre le total territorial de ressources potentiel et réel est trop fort, une proportion  (self closureRate) du territoire est sélectionnée"
self assessAndSelectTerrTotConditions: riskyPlots.
"...et cette proportion est mise en défens pour la durée et les siaosns sélectionnées"
(riskyPlots isNotEmpty) ifTrue: [self protectTerrTot: riskyPlots.
"inscription d'un indicateur spatial repérant l'attribution du plot" self visualizingEnvRule]</body>

<body package="WitUses">plotTotClosing

"Repère tous les plots qui ont un total de ressources trop faible et en met une fraction en défens..
Au début de chaque année, ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total d'une ressource d'un plot (hors eau du sol, et pour l'instant _r) est inférieur à un minimum (attribut self minPlotRessCoeff), alors est mise en défens une fraction (attribut self closureRate) de l'ensemble des plots dont le total de ressources est ainsi inférieur au minimum requis, aux saisons sélectionnées (self seasonsClosing) pour x (self closureDuration) années"

"En début d'année, chaque plot en défens met à jour son statut de mise en défens"
((WitUses timeStep\\4)=1) ifTrue:[self updatingClosure.
"puis les plots ayant un trop grand écart (self minPlotRessCoeff) entre leur total potentiel et réel sont repérés"

(PolicyMaker policyDuration isNil) ifTrue: [self selectPlotTotClosing]
"...si une policy est appliquée, selon le rythme de renouvellement de la policy"
ifFalse: [(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:[self selectPlotTotClosing]].
]</body>

<body package="WitUses">selectPlotRessClosing

"Repère tous les plots qui ont une de leurs ressources trop dégradée et en met une fraction en défens..
Au début de chaque année, ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total d'une ressource d'un plot (attribut self hotRessource), hors eau du sol, et pour l'instant _s, est inférieur à un minimum (attribut self minPlotRessCoeff), alors les usages utilisant cette ressource (et les usages dégradant cette ressource) sont interdits sur une fraction (attribut self closureRate) des plots dont la quantité de cette ressource est la plus faible, pour x années (attribut self closureDuration) et pour les saisons sélectionnées (attribut self closingResources)"

| riskyPlots |

riskyPlots:= OrderedCollection new.
self assessPlotRessCondition: riskyPlots.
"une proportion (self closureRate) de plots du groupe restant est mis en défens..."
"si pas d epolicy à chaque pas de temps,..."
(riskyPlots isNotEmpty) ifTrue: [self selectingPlots: riskyPlots].
"Les usages de la ressource concernée sont interdits sur une proportion (self closureRate) de plots en situation critique concernant la ressource sélectionnée"
(riskyPlots isNotEmpty) ifTrue: [self protectPlotRess: riskyPlots.
"inscription d'un indicateur spatial repérant l'attribution du plot"
self visualizingEnvRule].</body>

<body package="WitUses">plotRessClosing

"Repère tous les plots qui ont une de leurs ressources trop dégradée et en met une fraction en défens..
Au début de chaque année, ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total d'une ressource d'un plot (attribut self hotRessource), hors eau du sol, et pour l'instant _s, est inférieur à un minimum (attribut self minPlotRessCoeff), alors les usages utilisant cette ressource (et les usages dégradant cette ressource) sont interdits sur une fraction (attribut self closureRate) des plots dont la quantité de cette ressource est la plus faible, pour x années (attribut self closureDuration) et pour les saisons sélectionnées (attribut self closingResources)"

"En début d'année, chaque plot en défens met à jour son statut de mise en défens"
((WitUses timeStep\\4)=1) ifTrue:[self updatingClosure.
"puis au sujet de la ressource sélectionnée (self hotRessource) les plots ayant un trop grand écart (self minPlotRessCoeff) entre leur total potentiel et réel sont sélectionnés"
(PolicyMaker policyDuration isNil) ifTrue: [self selectPlotRessClosing]
"...si une policy est appliquée, selon le rythme de renouvellement de la policy"
ifFalse: [(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:[self selectPlotRessClosing]].
]</body>

<body package="WitUses">terrRessClosing

"Vison globale des quantités totales de chaque ressource au niveau territorial (pas un indicateur par plot).
Au début de chaque année, ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total d'une ressource du territoire (hors eau du sol, et pour l'instant _s) est inférieur à un minimum (attribut self minPlotRessCoeff), alors l'usage de cette ressource (et les usages dégradant cette ressource) sont interdits sur une fraction (attribut self closureRate) des plots les moins fournis concernant cette ressource, aux saisons sélectionnées (attribut self closingResources) et pour x années (attribut self closureDuration)"

"En début d'année, chaque plot en défens met à jour son statut de mise en défens"
((WitUses timeStep\\4)=1) ifTrue:[self updatingClosure.
"puis au sujet de la ressource sélectionnée (self hotRessource) les plots ayant un trop grand écart (self minPlotRessCoeff) entre leur total potentiel et réel sont repérés"
(PolicyMaker policyDuration isNil) ifTrue: [self selectTerrRessClosing]
"...si une policy est appliquée, selon le rythme de renouvellement de la policy"
ifFalse: [(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:[self selectTerrRessClosing]].
]</body>

<body package="WitUses">identifyingRessCrisis

| initRessSrTerr initRessRsTerr initRess_rTerr potentialRessSrTerr potentialRessRsTerr potentialRess_rTerr initRessTerr potentialRessTerr |

"On repère d'abord si crise en ressources"
initRessSrTerr:=0. initRessRsTerr:=0. initRess_rTerr:=0. potentialRessSrTerr:=0. potentialRessRsTerr:=0. potentialRess_rTerr:=0.
self class myTerritory do: [:p |
initRessSrTerr:= initRessSrTerr + p initRessSr. initRessRsTerr:= initRessRsTerr + p initRessRs. initRess_rTerr:= initRess_rTerr + p initRess_r. potentialRessSrTerr:= potentialRessSrTerr+p potentialRessSr. potentialRessRsTerr:= potentialRessRsTerr+p potentialRessRs. potentialRess_rTerr:= potentialRess_rTerr+p potentialRess_r].
initRessTerr:= initRessSrTerr + initRessRsTerr + initRess_rTerr. potentialRessTerr:= potentialRessSrTerr + potentialRessRsTerr + potentialRess_rTerr.
(initRessTerr &lt; (self class minPlotRessCoeff * potentialRessTerr))
ifTrue: [
"et si crise, on sélectionne alors les plots aux ressources clés"
self selectingHotPlots]</body>

<body package="WitUses">terrTotClosing

"Vision globale de la quantité totale de ressources au niveau territorial (pas un indicateur par plot).
Au début de chaque année ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total des ressources du territoire (hors eau du sol, et pour l'instant _s) est inférieur à un minimum (attribut self minPlotRessCoeff), alors sont mis en défens pour x années (attribut self closureDuration) et sur les saisons sélectionnées (attribut self closingSeasons) une fraction (attribut self closureRate) des plots les moins fournis en ressources"

"En début d'année, chaque plot en défens met à jour son statut de mise en défens"
((WitUses timeStep\\4)=1) ifTrue:[self updatingClosure.
"puis si l'écart (self minPlotRessCoeff) entre le total territorial de ressources potentiel et réel est trop fort, une proportion  (self closureRate) du territoire est sélectionnée"
(PolicyMaker policyDuration isNil) ifTrue: [self selectTerrTotClosing]
"...si une policy est appliquée, selon le rythme de renouvellement de la policy"
ifFalse: [(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:[self selectTerrTotClosing]].
]</body>

<body package="WitUses">selectPlotTotClosing

"Repère tous les plots qui ont un total de ressources trop faible et en met une fraction en défens..
Au début de chaque année, ET AVANT LE RENOUVELLEMENT DES RESSOURCES, si le total d'une ressource d'un plot (hors eau du sol, et pour l'instant _r) est inférieur à un minimum (attribut self minPlotRessCoeff), alors est mise en défens une fraction (attribut self closureRate) de l'ensemble des plots dont le total de ressources est ainsi inférieur au minimum requis, aux saisons sélectionnées (self seasonsClosing) pour x (self closureDuration) années"

| riskyPlots |

riskyPlots:= OrderedCollection new.
self class myTerritory do: [:p |
((p initRessSr + p initRess_r + p initRessRs) &lt; (self class minPlotRessCoeff * (p potentialRessSr + p potentialRess_r + p potentialRessRs))) ifTrue: [riskyPlots add:p.
"et une proportion (self closureRate) de plots du groupe restant est mis en défens"
self selectingPlots: riskyPlots.
self protectPlotTot: riskyPlots.
"inscription d'un indicateur spatial repérant l'attribution du plot"
self visualizingEnvRule]]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>socialRules</category>

<body package="WitUses">livingStandard

"En début d'année, si le capital d'un user est inférieur à un minimum vital (livingStandard de Calibration), alors il est prioritaire pour ses affectations SUR DES TERRES LIBRES"
(PolicyMaker policyDuration isNil)
ifTrue:
[((WitUses timeStep\\4)=1) ifTrue:["self myUsers do: [:u | ((u capital + Calibration livingStandard) &lt; 0) ifTrue: [self priorityUsers add: u.]]]" self priorityUsers: (self myUsers select: [:u | (u capital + Calibration livingStandard) &lt; 0])]]
ifFalse:
[(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:["self myUsers do: [:u | ((u capital + Calibration livingStandard) &lt; 0) ifTrue: [self priorityUsers add: u.]]]" self priorityUsers: (self myUsers select: [:u | (u capital + Calibration livingStandard) &lt; 0])]].
(self priorityUsers isEmpty) ifFalse: [self halt].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RulesImplementer</class-id> <category>implementingSteps</category>

<body package="WitUses">updatingClosure
	
"Chaque plot en défens met à jour son statut de mise en défens"
self class myTerritory do: [:p | (p closureDuration &gt;1) ifTrue: [p closureDuration: p closureDuration-1] ifFalse: [
"et si la durée de mise en défens est achevée, se libère de la closure"
(p closureDuration =1) ifTrue: [p closureDuration:0. p holder: nil.
self mySeasonPlotUses do: [:spu | ((spu hostedPlot = p) and: [spu currentUsers includes: self]) ifTrue: [spu currentUsers remove: self]]]]].</body>

<body package="WitUses">protectPlotTot: riskyPlots

"une proportion (self closureRate) de plots du groupe restant est mis en défens"

self mySeasonPlotUses do: [:spu | ((riskyPlots includes: spu hostedPlot) and: [self closingSeasons includes: spu season asNumber]) ifTrue: [spu currentUsers removeAll. spu currentUsers add: self]].</body>

<body package="WitUses">selectingPlots: riskyPlots

"une proportion (self closureRate) de plots du groupe restant est mis en défens"

| rate selectedPlots |

"on enlève ceux qui sont déjà occupés"
(self coveringHoldedPlots = 'No') ifTrue: [riskyPlots removeAllSuchThat: [:p | (p holder notNil)]].
 
"et on met en défens une fraction des plots restants"
selectedPlots:=OrderedCollection new.
rate:= (self class myTerritory size*RulesImplementer closureRate) asInteger.

selectedPlots:= (Cormas selectRandomlyFrom: riskyPlots subCollectionSize: rate) do: [:plot | plot closureDuration:self closureDuration]. (selectedPlots isNotEmpty) ifTrue: [riskyPlots removeAll. riskyPlots addAll: selectedPlots].</body>

<body package="WitUses">assessAndSelectTerrRessConditions: riskyPlots

"Selon la ressource sélectionnée (self hotResource), sélection de splots dont l'écart entre ressources init et potentielles est trop élevé"

| initRessSrTerr initRessRsTerr initRess_rTerr potentialRessSrTerr potentialRessRsTerr potentialRess_rTerr sortedPlots rate availablePlots |

initRessSrTerr:=0. initRessRsTerr:=0. initRess_rTerr:=0. potentialRessSrTerr:=0. potentialRessRsTerr:=0. potentialRess_rTerr:=0.

self class myTerritory do: [:p |
initRessSrTerr:= initRessSrTerr + p initRessSr. initRessRsTerr:= initRessRsTerr + p initRessRs. initRess_rTerr:= initRess_rTerr + p initRess_r. potentialRessSrTerr:= potentialRessSrTerr+p potentialRessSr. potentialRessRsTerr:= potentialRessRsTerr+p potentialRessRs. potentialRess_rTerr:= potentialRess_rTerr+p potentialRess_r].

"Selon la ressource sélectionnée (self hotResource), si l'écart territorial est trop grand, les plots sont classés..."
availablePlots:= self class myTerritory select: [:p | p holder isNil].
(self hotResource = 'Sr') ifTrue: [(initRessSrTerr &lt; (RulesImplementer minPlotRessCoeff * potentialRessSrTerr)) ifTrue: [sortedPlots:= availablePlots asSortedCollection: [:plot1 :plot2 | plot1 initRessSr &gt; plot2 initRessSr]]].
(self hotResource = 'Rs') ifTrue: [(initRessRsTerr &lt; (RulesImplementer minPlotRessCoeff * potentialRessRsTerr)) ifTrue: [sortedPlots:= availablePlots asSortedCollection: [:plot1 :plot2 | plot1 initRessRs &gt; plot2 initRessRs]]].
(self hotResource = '_r') ifTrue: [(initRess_rTerr &lt; (RulesImplementer minPlotRessCoeff * potentialRess_rTerr)) ifTrue: [sortedPlots:= availablePlots asSortedCollection: [:plot1 :plot2 | plot1 initRess_r &gt; plot2 initRess_r]]].
"...et la fraction (rate) la plus dégradée concernant cette ressource est sélectionnée' "
(sortedPlots isNil) ifFalse: [rate:= (RulesImplementer closureRate * 144) asInteger. sortedPlots size - rate to: sortedPlots size do: [:plot | riskyPlots add: (sortedPlots at: plot)]].</body>

<body package="WitUses">protectTerrTot: riskyPlots

"...et cette proportion est mise en défens pour la durée et les siaosns sélectionnées"

self class myTerritory do: [:p | (riskyplots includes: p) ifTrue: [p closureDuration:self closureDuration]].
self mySeasonPlotUses do: [:spu |
(self closingSeasons includes: spu season asNumber) ifTrue: [(riskyplots includes:spu hostedPlot) ifTrue: [spu currentUsers removeAll. spu currentUsers add: self]]
].</body>

<body package="WitUses">protectTerrRess: riskyPlots

self class myTerritory do: [:p | (riskyPlots includes: p) ifTrue: [p closureDuration:self closureDuration]].
self mySeasonPlotUses do: [:spu |
 (self closingSeasons includes: spu season asNumber) ifTrue: [
(self hotResource = 'Sr') ifTrue: [((riskyPlots includes: spu hostedPlot) and:[spu use= 'Settled']) ifTrue: [spu currentUsers removeAll. spu currentUsers add:self]].
(self hotResource = 'Rs') ifTrue: [((riskyPlots includes: spu hostedPlot) and:[spu use~ 'Picking']) ifTrue: [spu currentUsers removeAll. spu currentUsers add:self]].
(self hotResource = '_r') ifTrue: [((riskyPlots includes: spu hostedPlot) and:[spu use~ 'Mobile']) ifTrue: [spu currentUsers removeAll. spu currentUsers add:self]]
]].</body>

<body package="WitUses">assessPlotRessCondition: riskyPlots 

"Selon la ressource sélectionnée (self hotResource), sélection de splots dont  l'écart entre ressources init et potentielles est trop élevé"

self class myTerritory do: [:p |
(((self hotResource = 'Sr') and: [p initRessSr &lt; (RulesImplementer minPlotRessCoeff * p potentialRessSr)])
or: [((self hotResource = 'Rs') and: [p initRessRs &lt; (RulesImplementer minPlotRessCoeff * p potentialRessRs)])])
ifTrue: [riskyPlots add:p].
((self hotResource = '_r') and: [p initRess_r &lt; (RulesImplementer minPlotRessCoeff * p potentialRess_r)]) ifTrue: [riskyPlots add:p]
].</body>

<body package="WitUses">assessAndSelectTerrTotConditions: riskyPlots

"l'écart (self minPlotRessCoeff) entre le total territorial de ressources potentiel et réel est calculé, et s'il est trop fort, une proportion du territoire est sélection pour protection"

| initRessPlotTot initRessTerrTot potentialRessPlotTot potentialRessTerrTot sortedPlots availablePlots rate |

 initRessTerrTot:=0. potentialRessTerrTot:=0. sortedPlots:= IndexedSet new.

"L'écart (self minPlotRessCoeff) entre le total territorial de ressources potentiel et réel est calculé"
self class myTerritory do: [:p |
potentialRessPlotTot:= p potentialRessSr + p potentialRess_r + p potentialRessRs. potentialRessTerrTot:= potentialRessTerrTot+potentialRessPlotTot.
initRessPlotTot:= p initRessSr + p initRess_r + p initRessRs. initRessTerrTot:= initRessTerrTot+initRessPlotTot].
"si cet écart est trop grand, les plots qui ne sont pas déjà en défens ou qui ne sont pas déjà privatisés..."
(initRessTerrTot &lt; (RulesImplementer minPlotRessCoeff * potentialRessTerrTot)) ifTrue: [
availablePlots:= self class myTerritory select: [:plot | plot holder isNil].
"...et une proportion (self closureRate) des plots du territoire les moins fournis en ressources sont mis en défens"
rate:= (RulesImplementer closureRate * 144) asInteger.
sortedPlots:= availablePlots asSortedCollection: [:plot1 :plot2 | (plot1 initRessSr + plot1 initRessRs +plot1 initRess_r) &gt; (plot2 initRessSr + plot2 initRessRs +plot2 initRess_r)].
sortedPlots size - rate to: sortedPlots size do: [:plot | riskyPlots add: (sortedPlots at: plot)].
																		].</body>

<body package="WitUses">protectPlotRess: riskyPlots

"Les usages de la ressource concernée sont interdits sur une proportion (self closureRate) de plots en situation critique concernant la ressource sélectionnée, pour les siasons sélectionnées.
Nb: pour Rs et _r, avec en plus interdiction de Settled, qui n'exploite pas mais dégrade (défriche) la ressource"

self mySeasonPlotUses do: [:spu |
((riskyPlots includes: spu hostedPlot) and: [self closingSeasons includes: spu season asNumber]) ifTrue:[
(self hotResource = 'Sr') ifTrue: [(spu use= 'Settled') ifTrue: [spu currentUsers removeAll. spu currentUsers add:self]].
(self hotResource = 'Rs') ifTrue: [(spu use~ 'Picking') ifTrue: [spu currentUsers removeAll. spu currentUsers add:self]].
(self hotResource = '_r') ifTrue: [(spu use~ 'Mobile') ifTrue: [spu currentUsers removeAll. spu currentUsers add:self]].
]].</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.NorthEast class</class-id> <category>+ parameter unit</category>

<body package="WitUses">landUsesPolicy_type 
	"Return the type of attribute #landUsesPolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">landUsesRule_type 
	"Return the type of attribute #landUsesRule.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthEast class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthEast class</class-id> <category>accessing</category>

<body package="WitUses">myTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myTerritory ifNil:[myTerritory := OrderedCollection new]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthEast class</class-id> <category>- default value</category>

<body package="WitUses">landUsesRule_default 
	"Return the default value of attribute #landUsesRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">landUsesPolicy_default 
	"Return the default value of attribute #landUsesPolicy : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthEast class</class-id> <category>As yet unclassified</category>

<body package="WitUses">mySeasonPlotUses
	self halt</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.NorthEast</class-id> <category>control</category>

<body package="WitUses">initMyTerritory
	"comment stating purpose of message"</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthEast</class-id> <category>accessing</category>

<body package="WitUses">landUsesRule: aByteString 
	"Setter accessor of attribute landUsesRule	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landUsesRule = aByteString ifTrue:[^nil].   "for optimization"
	landUsesRule := aByteString. 
	self changed</body>

<body package="WitUses">envRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^envRule ifNil:[envRule := '']</body>

<body package="WitUses">landTenureRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landTenureRule ifNil:[landTenureRule := '']</body>

<body package="WitUses">backUpOfAllSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpOfAllSeasonPlotUses ifNil:[backUpOfAllSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">landUsesRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landUsesRule ifNil:[landUsesRule := '']</body>

<body package="WitUses">sahelianRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sahelianRule ifNil:[sahelianRule := '']</body>

<body package="WitUses">backUpAppliedRules
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpAppliedRules ifNil:[backUpAppliedRules := OrderedCollection new]</body>

<body package="WitUses">mySeasonRessProviderUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonRessProviderUses ifNil:[mySeasonRessProviderUses := OrderedCollection new]</body>

<body package="WitUses">socialRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^socialRule ifNil:[socialRule := '']</body>

<body package="WitUses">backUpMyTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpMyTerritory ifNil:[backUpMyTerritory := OrderedCollection new]</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SouthEast class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SouthEast class</class-id> <category>accessing</category>

<body package="WitUses">myTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myTerritory ifNil:[myTerritory := OrderedCollection new]</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SouthEast</class-id> <category>control</category>

<body package="WitUses">initMyTerritory
	"comment stating purpose of message"</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SouthEast</class-id> <category>accessing</category>

<body package="WitUses">mySeasonRessProviderUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonRessProviderUses ifNil:[mySeasonRessProviderUses := OrderedCollection new]</body>

<body package="WitUses">socialRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^socialRule ifNil:[socialRule := '']</body>

<body package="WitUses">backUpMyTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpMyTerritory ifNil:[backUpMyTerritory := OrderedCollection new]</body>

<body package="WitUses">envRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^envRule ifNil:[envRule := '']</body>

<body package="WitUses">landTenureRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landTenureRule ifNil:[landTenureRule := '']</body>

<body package="WitUses">backUpOfAllSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpOfAllSeasonPlotUses ifNil:[backUpOfAllSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">landUsesRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landUsesRule ifNil:[landUsesRule := '']</body>

<body package="WitUses">sahelianRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sahelianRule ifNil:[sahelianRule := '']</body>

<body package="WitUses">backUpAppliedRules
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpAppliedRules ifNil:[backUpAppliedRules := OrderedCollection new]</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.EnVariability class</class-id> <category>+ parameter unit</category>

<body package="WitUses">nberTempRessProv_type 
	"Return the type of attribute #nberTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">mySeasonPlots_type 
	"Return the type of attribute #mySeasonPlots.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">myTerritory_type 
	"Return the type of attribute #myTerritory.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">usersNber_unit 
	"Return the unit of attribute #usersNber.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">fire_type 
	"Return the type of attribute #fire.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">occuringFire_type 
	"Return the type of attribute #occuringFire.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">ressProviderTypeOneRatio_type 
	"Return the type of attribute #ressProviderTypeOneRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">stockSteady_type 
	"Return the type of attribute #stockSteady.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscape_III_Ratio_type 
	"Return the type of attribute #landscape_III_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">useCoeffRSTemp_unit 
	"Return the unit of attribute #useCoeffRSTemp.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscape_I_Ratio_unit 
	"Return the unit of attribute #landscape_I_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscape_II_Ratio_unit 
	"Return the unit of attribute #landscape_II_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscape_III_Ratio_unit 
	"Return the unit of attribute #landscape_III_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">useCoefRSteady_unit 
	"Return the unit of attribute #useCoefRSteady.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">NberSteadyRessProv_type 
	"Return the type of attribute #NberSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscape_IV_Ratio_unit 
	"Return the unit of attribute #landscape_IV_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">NberTempRessProv_unit 
	"Return the unit of attribute #NberTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">NberSteadyRessProv_unit 
	"Return the unit of attribute #NberSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ratioSteadyRessProv_type 
	"Return the type of attribute #ratioSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landScapeIIIRessValue_unit 
	"Return the unit of attribute #landScapeIIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landScapeIIIRessValue_type 
	"Return the type of attribute #landScapeIIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">NberTempRessProv_type 
	"Return the type of attribute #NberTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscape_I_ratio_unit 
	"Return the unit of attribute #landscape_I_ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">nberSteadyRessProv_type 
	"Return the type of attribute #nberSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">stockSteady_unit 
	"Return the unit of attribute #stockSteady.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">usersNber_type 
	"Return the type of attribute #usersNber.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landScapeIIRessValue_unit 
	"Return the unit of attribute #landScapeIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">nberTempRessProv_unit 
	"Return the unit of attribute #nberTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">mySeasonPlotUses_type 
	"Return the type of attribute #mySeasonPlotUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">ratioTempRessProv_unit 
	"Return the unit of attribute #ratioTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">useCoeffRSTemp_type 
	"Return the type of attribute #useCoeffRSTemp.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">myRessProviders_type 
	"Return the type of attribute #myRessProviders.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">landscape_II_Ratio_type 
	"Return the type of attribute #landscape_II_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscape_I_ratio_type 
	"Return the type of attribute #landscape_I_ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">ratioSteadyRessProv_unit 
	"Return the unit of attribute #ratioSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressProviderTypeTwoRatio_type 
	"Return the type of attribute #ressProviderTypeTwoRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">useCoefRSteady_type 
	"Return the type of attribute #useCoefRSteady.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">nberSteadyRessProv_unit 
	"Return the unit of attribute #nberSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscape_I_Ratio_type 
	"Return the type of attribute #landscape_I_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ratioTempRessProv_type 
	"Return the type of attribute #ratioTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">myRessProvider_type 
	"Return the type of attribute #myRessProvider.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">landScapeIIRessValue_type 
	"Return the type of attribute #landScapeIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ressProviderTypeOneRatio_unit 
	"Return the unit of attribute #ressProviderTypeOneRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressProviderTypeTwoRatio_unit 
	"Return the unit of attribute #ressProviderTypeTwoRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscape_IV_Ratio_type 
	"Return the type of attribute #landscape_IV_Ratio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.EnVariability class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.EnVariability class</class-id> <category>accessing</category>

<body package="WitUses">nberSteadyRessProv: aSmallInteger 
	"Setter accessor of attribute nberSteadyRessProv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nberSteadyRessProv := aSmallInteger</body>

<body package="WitUses">nberTempRessProv
	"Getter accessor with initial value = 5. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nberTempRessProv ifNil:[nberTempRessProv := 5]</body>

<body package="WitUses">nberSteadyRessProv
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nberSteadyRessProv ifNil:[nberSteadyRessProv := 0]</body>

<body package="WitUses">nberTempRessProv: aSmallInteger 
	"Setter accessor of attribute nberTempRessProv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nberTempRessProv := aSmallInteger</body>

<body package="WitUses">usersNber: aSmallInteger 
	"Setter accessor of attribute usersNber	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	usersNber = aSmallInteger ifTrue:[^nil].   "for optimization"
	usersNber := aSmallInteger. 
	self changed</body>

<body package="WitUses">usersNber
	"Getter accessor with initial value = 15. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^usersNber ifNil:[usersNber := 15]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.EnVariability class</class-id> <category>- default value</category>

<body package="WitUses">stockSteady_default 
	"Return the default value of attribute #stockSteady : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">useCoefRSTemp_default 
	"Return the default value of attribute #useCoefRSTemp : 5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^5</body>

<body package="WitUses">landscape_III_Ratio_default 
	"Return the default value of attribute #landscape_III_Ratio : 65.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^65</body>

<body package="WitUses">landScapeIIRessValue_default 
	"Return the default value of attribute #landScapeIIRessValue : 2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^2</body>

<body package="WitUses">occuringFire_default 
	"Return the default value of attribute #occuringFire : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">mySeasonPlotUses_default 
	"Return the default value of attribute #mySeasonPlotUses : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">nberTempRessProv_default 
	"Return the default value of attribute #nberTempRessProv : 5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^5</body>

<body package="WitUses">mySeasonPlots_default 
	"Return the default value of attribute #mySeasonPlots : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">nberSteadyRessProv_default 
	"Return the default value of attribute #nberSteadyRessProv : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ressProviderTypeTwoRatio_default 
	"Return the default value of attribute #ressProviderTypeTwoRatio : 12.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^12</body>

<body package="WitUses">useCoefRSteady_default 
	"Return the default value of attribute #useCoefRSteady : 100.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^100</body>

<body package="WitUses">NberSteadyRessProv_default 
	"Return the default value of attribute #NberSteadyRessProv : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">landscape_I_ratio_default 
	"Return the default value of attribute #landscape_I_ratio : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">useCoeffRSTemp_default 
	"Return the default value of attribute #useCoeffRSTemp : 5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^5</body>

<body package="WitUses">usersNber_default 
	"Return the default value of attribute #usersNber : 15.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^15</body>

<body package="WitUses">myRessProvider_default 
	"Return the default value of attribute #myRessProvider : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">myTerritory_default 
	"Return the default value of attribute #myTerritory : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">landScapeIIIRessValue_default 
	"Return the default value of attribute #landScapeIIIRessValue : 3.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^3</body>

<body package="WitUses">landscape_IV_Ratio_default 
	"Return the default value of attribute #landscape_IV_Ratio : 14.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^14</body>

<body package="WitUses">landscape_I_Ratio_default 
	"Return the default value of attribute #landscape_I_Ratio : 27.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^27</body>

<body package="WitUses">ratioSteadyRessProv_default 
	"Return the default value of attribute #ratioSteadyRessProv : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ressProviderTypeOneRatio_default 
	"Return the default value of attribute #ressProviderTypeOneRatio : 15.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^15</body>

<body package="WitUses">NberTempRessProv_default 
	"Return the default value of attribute #NberTempRessProv : 3.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^3</body>

<body package="WitUses">fire_default 
	"Return the default value of attribute #fire : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">landscape_II_Ratio_default 
	"Return the default value of attribute #landscape_II_Ratio : 38.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^38</body>

<body package="WitUses">ratioTempRessProv_default 
	"Return the default value of attribute #ratioTempRessProv : 3.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^3</body>

<body package="WitUses">randomAttributes 
	"Returns a list of randomAttributes: a List of arrays: [#(#attributeName scopeLevel #randFloat min max) #(...) #(...) ... ].

	This method is automatically redefined when creating new random attributes" 	
	| list |
	list := List new. 
	list add: #(#climateSpatialVariability #instance #randomInteger 0 10). 
	list add: #(#climateUncertainty #instance #randomInteger 0 10). 
	^list</body>

<body package="WitUses">myRessProviders_default 
	"Return the default value of attribute #myRessProviders : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.EnVariability</class-id> <category>initialize-release</category>

<body package="WitUses">initRess

	self myTerritory do: [:p | 
			(p landscape = 'I') ifTrue:[
p initRessRS: Calibration landscapeIRessValue * Calibration ressCoeff. p remainRessRS: p initRessRS.
p initRessRs: Calibration landscapeIRessValue * Calibration ressCoeff. p remainRessRs: p initRessRs.
p initRessSr: Calibration landscapeIRessValue * Calibration ressCoeff. p remainRessSr: p initRessSr.
p initRess_r: Calibration landscapeIRessValue * Calibration ressCoeff. p remainRess_r: p initRess_r.
p initRess_s: Calibration landscapeIRessValue * Calibration ressCoeff. p remainRess_s: p initRess_s].
			(p landscape = 'II') ifTrue:[
p initRessRS: Calibration landscapeIIRessValue * Calibration ressCoeff. p remainRessRS: p initRessRS.
p initRessRs: Calibration landscapeIIRessValue * Calibration ressCoeff. p remainRessRs: p initRessRs.
p initRessSr: Calibration landscapeIIRessValue *Calibration ressCoeff. p remainRessSr: p initRessSr.
p initRess_r: Calibration landscapeIIRessValue *Calibration ressCoeff. p remainRess_r: p initRess_r.
p initRess_s: Calibration landscapeIIRessValue *Calibration ressCoeff. p remainRess_s: p initRess_s].
			(p landscape = 'III') ifTrue:[
p initRessRS: Calibration landscapeIIIRessValue * Calibration ressCoeff. p remainRessRS: p initRessRS.
p initRessRs: Calibration landscapeIIIRessValue * Calibration ressCoeff. p remainRessRs: p initRessRs. 
p initRessSr: Calibration landscapeIIIRessValue * Calibration ressCoeff. p remainRessSr: p initRessSr.
p initRess_r: Calibration landscapeIIIRessValue * Calibration ressCoeff. p remainRess_r: p initRess_r.
p initRess_s: Calibration landscapeIIIRessValue * Calibration ressCoeff. p remainRess_s: p initRess_s].
			(p landscape = 'IV') ifTrue:[
p initRessRS: Calibration landscapeIVRessValue * Calibration ressCoeff. p remainRessRS: p initRessRS.
p initRessRs: Calibration landscapeIVRessValue * Calibration ressCoeff. p remainRessRs: p initRessRs.
p initRessSr: Calibration landscapeIVRessValue * Calibration ressCoeff. p remainRessSr: p initRessSr.
p initRess_r: Calibration landscapeIVRessValue * Calibration ressCoeff. p remainRess_r: p initRess_r.
p initRess_s: Calibration landscapeIVRessValue * Calibration ressCoeff. p remainRess_s: p initRess_s].
		].
self myRessProviders do: [:rp | 
(rp providerType = '1') ifTrue:[rp initRess: Calibration ressOfTempRessProv * Calibration ressCoeff. rp remainRess: rp initRess].
(rp providerType = '2') ifTrue:[rp initRess: Calibration ressOfSteadyRessProv * Calibration ressCoeff. rp remainRess: rp initRess].
						].
"enregistre en mémoire les ressources potentielles au départ de la simulation (qui seront ensuite modifiées chaque année en fonction du climat de l'année"
self myTerritory do: [:p | p potentialRessRS: p initRessRS. p potentialRessRs: p initRessRs. p potentialRessSr: p initRessSr. p potentialRess_r: p initRess_r. p potentialRess_s: p initRess_s].
self myRessProviders do: [:rp | rp potentialRess: rp initRess].</body>

<body package="WitUses">initLandscapes
	
	(Cormas
		selectRandomlyFrom: self myTerritory
		subCollectionSize: self landscape_I_Ratio
		constrainedBy: [:p | p landscape isNil])
		do: [:onePlot | onePlot landscape: 'I'].
	(Cormas
		selectRandomlyFrom: self myTerritory
		subCollectionSize: self landscape_II_Ratio
		constrainedBy: [:p | p landscape isNil])
		do: [:onePlot | onePlot landscape: 'II'].
	(Cormas
		selectRandomlyFrom: self myTerritory
		subCollectionSize: self landscape_III_Ratio
		constrainedBy: [:p | p landscape isNil])
		do: [:onePlot | onePlot landscape: 'III'].
	(Cormas
		selectRandomlyFrom: self myTerritory
		subCollectionSize: self landscape_IV_Ratio
		constrainedBy: [:p | p landscape isNil])
		do: [:onePlot | onePlot landscape: 'IV']</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.EnVariability</class-id> <category>accessing</category>

<body package="WitUses">myUsedSeason1Plots
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonPlotUses ifNil:[mySeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">landscape_III_Ratio
	"Getter accessor with initial value = 65. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscape_III_Ratio ifNil:[landscape_III_Ratio := 65]</body>

<body package="WitUses">mySeasonPlotUses: aOrderedCollection 
	"Setter accessor of attribute mySeasonPlotUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	mySeasonPlotUses := aOrderedCollection</body>

<body package="WitUses">mySeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonPlotUses ifNil:[mySeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">myRessProviders: aOrderedCollection 
	"Setter accessor of attribute myRessProviders	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myRessProviders := aOrderedCollection</body>

<body package="WitUses">landscape_IV_Ratio: aSmallInteger 
	"Setter accessor of attribute landscape_IV_Ratio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscape_IV_Ratio := aSmallInteger</body>

<body package="WitUses">landscape_III_Ratio: aSmallInteger 
	"Setter accessor of attribute landscape_III_Ratio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscape_III_Ratio := aSmallInteger</body>

<body package="WitUses">myRessProviders
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myRessProviders ifNil:[myRessProviders := OrderedCollection new]</body>

<body package="WitUses">myRessProviderUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myRessProviderUses ifNil:[myRessProviderUses := OrderedCollection new]</body>

<body package="WitUses">landscape_II_Ratio
	"Getter accessor with initial value = 38. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscape_II_Ratio ifNil:[landscape_II_Ratio := 38]</body>

<body package="WitUses">landscape_II_Ratio: aSmallInteger 
	"Setter accessor of attribute landscape_II_Ratio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscape_II_Ratio := aSmallInteger</body>

<body package="WitUses">landscape_I_Ratio
	"Getter accessor with initial value = 27. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscape_I_Ratio ifNil:[landscape_I_Ratio := 27]</body>

<body package="WitUses">myTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myTerritory ifNil:[myTerritory := OrderedCollection new]</body>

<body package="WitUses">landscape_IV_Ratio
	"Getter accessor with initial value = 14. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscape_IV_Ratio ifNil:[landscape_IV_Ratio := 14]</body>

<body package="WitUses">myRessProviderUses: aOrderedCollection 
	"Setter accessor of attribute myRessProvider	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myRessProviderUses := aOrderedCollection</body>

<body package="WitUses">landscape_I_Ratio: aSmallInteger 
	"Setter accessor of attribute landscape_I_Ratio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscape_I_Ratio := aSmallInteger</body>

<body package="WitUses">myUsedSeason1Plots: aOrderedCollection 
	"Setter accessor of attribute mySeasonPlots	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	mySeasonPlotUses := aOrderedCollection</body>

<body package="WitUses">myTerritory: aOrderedCollection 
	"Setter accessor of attribute myTerritory	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myTerritory := aOrderedCollection</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.EnVariability</class-id> <category>control</category>

<body package="WitUses">dryingSoils

"dès le début de la saison sèche (saison 2), les sols légers (landscapes I à III) in'ont  plus assez d'eau pour la culture mais peut abreuver deux troupeaux"
((WitUses timeStep\\4)=2) ifTrue:[self myTerritory do: [:p | (p landscape = 'IV') ifFalse: [(p remainRessRS&gt;=SettledUse needRS) ifTrue: [p remainRessRS: (MobileUse needRS*2)]]]].
"et les terres dégradées (landscapes I et II) perdent leurs potentialités agronomiques, même si pas cultivées en saison des pluies."
((WitUses timeStep\\4)=2) ifTrue:[self myTerritory do: [:p | ((p landscape = 'II') or: [p landscape = 'I']) ifTrue: [(p remainRessSr:0)]]].
"un peu plus tard (saison 3), même les sols de bas-fonds (landscapes IV) n'ont  plus assez d'eau pour la culture mais peut abreuver deux troupeaux"
((WitUses timeStep\\4)=3) ifTrue:[self myTerritory do: [:p | (p landscape = 'IV') ifTrue: [(p remainRessRS&gt;=SettledUse needRS) ifTrue: [p remainRessRS:(MobileUse needRS*2)]]]].
"En fin de saison sèche (saison 4), il n'y a plus du tout d'eau dans le sol"
((WitUses timeStep\\4)=0) ifTrue:[self myTerritory do: [:p | p remainRessRS:0]].</body>

<body package="WitUses">noClimateUncertainty

| geographicVariability weatherUncertainity |

weatherUncertainity:= Cormas selectRandomlyFrom: (List withAll: # (1)).

self myTerritory do: [:p |
(RulesImplementer class southEast includes: p) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(0)). p plotRessVariability: weatherUncertainity and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainity and:geographicVariability]].
(RulesImplementer class northEast includes: p) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(0)). p plotRessVariability: weatherUncertainity and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainity and:geographicVariability]].
(RulesImplementer class southWest includes: p) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(0)).p plotRessVariability: weatherUncertainity and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainity and:geographicVariability]].
(RulesImplementer class northWest includes: p) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(0)).p plotRessVariability: weatherUncertainity and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainity and:geographicVariability]]].</body>

<body package="WitUses">ressRenewing

((WitUses timeStep\\4) =1) ifTrue:[

"chaque plot enregistre la pression qu'il y a eu sur les ressources vivantes (Rs, Sr et _r)"
self myTerritory do: [:p | p overUseMemory].
"resilience la ressource d'un plot pas touché"
self resilience.
"self noClimateUncertainty."
"introduit l'incertitude climatique après deux premières années de calage"
(WitUses timeStep &gt;= 13) ifTrue:[self climateUncertainty].
"l'overuse impacte les ressources qui seront effectivement dispo (après uncertainty) pour la prochaine année"
self overUse.

"dynamique de base de renouvellement des ressources"
self myTerritory do: [:p |
	p remainRessRS: p initRessRS.
	p remainRessRs: p initRessRs.
	p remainRessSr: p initRessSr.
	p remainRess_r: p initRess_r.
	p remainRess_s: p initRess_s].
self myRessProviders do: [:rp |rp remainRess: rp initRess]].

"..;et enfin assèchement progessif au cours de l'année de l''eau contenu dans les sols"
self dryingSoils.</body>

<body package="WitUses">overUse

"surcharge"
self myTerritory do: [:p | (p overUseRs = MobileUse overYearlyThresholdRess) ifTrue: [p initRessRs: (MobileUse overRessUseRatio * p initRessRs). p memoOverUseRs: p memoOverUseRs + p overUseRs. p overUseRs: 0]].
self myTerritory do: [:p | (p overUseSr = SettledUse overYearlyThresholdRess) ifTrue: [p initRessSr: (SettledUse overRessUseRatio * p initRessSr). p memoOverUseSr: p memoOverUseSr + p overUseSr. p overUseSr: 0]].
self myTerritory do: [:p | (p overUse_r = PickUse overYearlyThresholdRess) ifTrue: [p initRess_r: (PickUse overRessUseRatio * p initRess_r). p memoOverUse_r: p memoOverUse_r + p overUseSr. p overUse_r: 0]].</body>

<body package="WitUses">resilience

"Lorsque le plot n'a pas été touché pendant l'année, s'il y a eu auparavant overuse, il diminue son overuse; et si son capital iinitial en ressources était inférieur au max théorique (potential), ce qui signifie qu'il avait été dégradé, il s'en ré-approche un peu"

self myTerritory do: [:p | (p remainRessSr = p initRessSr) ifTrue:[
(p memoOverUseSr &gt; 0) ifTrue: [p memoOverUseSr: p memoOverUseSr-1].
((p memoOverUseSr = 0) and: [p initRessSr &lt; p potentialRessSr]) ifTrue: [
((p initRessSr + (SettledUse overRessUseRatio/SettledUse resilienceRatioRess)) &lt; p potentialRessSr) ifTrue: [p initRessSr: p initRessSr + (SettledUse overRessUseRatio/SettledUse resilienceRatioRess)] ifFalse: [p initRessSr: p potentialRessSr]]]].

self myTerritory do: [:p | (p remainRessRs = p initRessRs) ifTrue:[
(p memoOverUseRs &gt; 0) ifTrue: [p memoOverUseRs: p memoOverUseRs-1].
((p memoOverUseRs = 0) and: [p initRessRs &lt; p potentialRessRs]) ifTrue: [
((p initRessRs + (MobileUse overRessUseRatio/MobileUse resilienceRatioRess)) &lt; p potentialRessRs) ifTrue: [p initRessRs: p initRessRs + (MobileUse overRessUseRatio/MobileUse resilienceRatioRess)] ifFalse: [p initRessRs: p potentialRessRs]]]].

self myTerritory do: [:p | (p remainRess_r = p initRess_r) ifTrue:[
(p memoOverUse_r &gt; 0) ifTrue: [p memoOverUse_r: p memoOverUse_r-1].
((p memoOverUse_r = 0) and: [p initRess_r &lt; p potentialRess_r]) ifTrue: [
((p initRess_r + (PickUse overRessUseRatio/PickUse resilienceRatioRess)) &lt; p potentialRess_r) ifTrue: [p initRess_r: p initRess_r + (PickUse overRessUseRatio/PickUse resilienceRatioRess)] ifFalse: [p initRess_r: p potentialRess_r]]]].</body>

<body package="WitUses">climateUncertainty

| geographicVariability weatherUncertainty |

weatherUncertainty:= Cormas selectRandomlyFrom: (List withAll: # (0.1 0.1 0.3 0.3 0.4 0.5 0.5 0.6 0.7 0.8 0.9)).

self myTerritory do: [:p |
((p x &gt; 6) and: [p y &gt; 6]) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(-0.5 -0.25 -0.1 -0.1 -0.1 0 0 0 0 0 0.1 0.1 0.1 0.25 0.5)). p plotRessVariability: weatherUncertainty and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainty and:geographicVariability]].
((p x &gt; 6) and: [p y &lt;= 6]) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(-0.5 -0.25 -0.1 -0.1 -0.1 0 0 0 0 0 0.1 0.1 0.1 0.25 0.5)). p plotRessVariability: weatherUncertainty and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainty and:geographicVariability]].
((p x &lt;= 6) and: [p y &gt; 6]) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(-0.5 -0.25 -0.1 -0.1 -0.1 0 0 0 0 0 0.1 0.1 0.1 0.25 0.5)).p plotRessVariability: weatherUncertainty and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainty and:geographicVariability]].
((p x &lt;= 6) and: [p y &lt;= 6]) ifTrue: [geographicVariability:= Cormas selectRandomlyFrom: (List withAll: #(-0.5 -0.25 -0.1 -0.1 -0.1 0 0 0 0 0 0.1 0.1 0.1 0.25 0.5)).p plotRessVariability: weatherUncertainty and:geographicVariability. self myRessProviders do: [:rp | rp ressVariability: weatherUncertainty and:geographicVariability]]].</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.Calibration class</class-id> <category>accessing</category>

<body package="WitUses">landscapeIIRessValue: aFloat 
	"Setter accessor of attribute landscapeIIRessValue	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscapeIIRessValue = aFloat ifTrue:[^nil].   "for optimization"
	landscapeIIRessValue := aFloat. 
	self changed</body>

<body package="WitUses">landscapeIIIRessValue: aSmallInteger 
	"Setter accessor of attribute landscapeIIIRessValue	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscapeIIIRessValue = aSmallInteger ifTrue:[^nil].   "for optimization"
	landscapeIIIRessValue := aSmallInteger. 
	self changed</body>

<body package="WitUses">ressOfTempRessProv
	"Getter accessor with initial value = 1.6. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressOfTempRessProv ifNil:[ressOfTempRessProv := 1.6]</body>

<body package="WitUses">ressCoeff: aFloat 
	"Setter accessor of attribute ressCoeff	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressCoeff := aFloat</body>

<body package="WitUses">landscapeIVRessValue
	"Getter accessor with initial value = 1.6. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscapeIVRessValue ifNil:[landscapeIVRessValue := 1.6]</body>

<body package="WitUses">livingStandard
	"Getter accessor with initial value = 2. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^livingStandard ifNil:[livingStandard := 2]</body>

<body package="WitUses">landscapeIRessValue
	"Getter accessor with initial value = 0.25. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscapeIRessValue ifNil:[landscapeIRessValue := 0.25]</body>

<body package="WitUses">livingStandard: aSmallInteger 
	"Setter accessor of attribute livingStandard	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	livingStandard := aSmallInteger</body>

<body package="WitUses">landscapeIIRessValue
	"Getter accessor with initial value = 0.5. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscapeIIRessValue ifNil:[landscapeIIRessValue := 0.5]</body>

<body package="WitUses">ressOfTempRessProv: aFloat 
	"Setter accessor of attribute ressOfTempRessProv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressOfTempRessProv := aFloat</body>

<body package="WitUses">ressOfSteadyRessProv
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressOfSteadyRessProv ifNil:[ressOfSteadyRessProv := 0]</body>

<body package="WitUses">stockRSTemp: aSmallInteger 
	"Setter accessor of attribute useCoeffRSTemp	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	useCoeffRSTemp := aSmallInteger</body>

<body package="WitUses">landscapeIIIRessValue
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landscapeIIIRessValue ifNil:[landscapeIIIRessValue := 1]</body>

<body package="WitUses">stockRSTemp
	"Getter accessor with initial value = 5. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^useCoeffRSTemp ifNil:[useCoeffRSTemp := 5]</body>

<body package="WitUses">landscapeIRessValue: aFloat 
	"Setter accessor of attribute landscapeIRessValue	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscapeIRessValue = aFloat ifTrue:[^nil].   "for optimization"
	landscapeIRessValue := aFloat. 
	self changed</body>

<body package="WitUses">userInitCapital: aSmallInteger 
	"Setter accessor of attribute userInitCapital	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	userInitCapital := aSmallInteger</body>

<body package="WitUses">userInitCapital
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^userInitCapital ifNil:[userInitCapital := 1]</body>

<body package="WitUses">ressOfSteadyRessProv: aSmallInteger 
	"Setter accessor of attribute ressOfSteadyRessProv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressOfSteadyRessProv := aSmallInteger</body>

<body package="WitUses">landscapeIVRessValue: aFloat 
	"Setter accessor of attribute landscapeIVRessValue	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscapeIVRessValue = aFloat ifTrue:[^nil].   "for optimization"
	landscapeIVRessValue := aFloat. 
	self changed</body>

<body package="WitUses">ressCoeff
	"Getter accessor with initial value = 0.7. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressCoeff ifNil:[ressCoeff := 0.7]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Calibration class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Calibration class</class-id> <category>+ parameter unit</category>

<body package="WitUses">overSoilYearlyThreshold_unit 
	"Return the unit of attribute #overSoilYearlyThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overUseThreshold_unit 
	"Return the unit of attribute #overUseThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">userInitCapital_type 
	"Return the type of attribute #userInitCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">minPlotRess_unit 
	"Return the unit of attribute #minPlotRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overUseYearlyThreshold_unit 
	"Return the unit of attribute #overUseYearlyThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscapeIRessValue_type 
	"Return the type of attribute #landscapeIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">overUseRessThreshold_type 
	"Return the type of attribute #overUseRessThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">ressOfSteadyRessProv_unit 
	"Return the unit of attribute #ressOfSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">RSUseCoeff_unit 
	"Return the unit of attribute #RSUseCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscapeIVRessValue_unit 
	"Return the unit of attribute #landscapeIVRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overSoilUseRatio_unit 
	"Return the unit of attribute #overSoilUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressCoeff_type 
	"Return the type of attribute #ressCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">ressOfSteadyRessProv_type 
	"Return the type of attribute #ressOfSteadyRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">minPlotRessRate_type 
	"Return the type of attribute #minPlotRessRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">overUseStepThreshold_unit 
	"Return the unit of attribute #overUseStepThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">minYearlyGrazed_unit 
	"Return the unit of attribute #minYearlyGrazed.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overSoilYearlyThreshold_type 
	"Return the type of attribute #overSoilYearlyThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overUseRessRatio_unit 
	"Return the unit of attribute #overUseRessRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">livingStandard_type 
	"Return the type of attribute #livingStandard.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overUseThreshold_type 
	"Return the type of attribute #overUseThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overUseRessRatio_type 
	"Return the type of attribute #overUseRessRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">landscapeIRessValue_unit 
	"Return the unit of attribute #landscapeIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressUseCoeff_unit 
	"Return the unit of attribute #ressUseCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">useCoeffRSTemp_unit 
	"Return the unit of attribute #useCoeffRSTemp.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overSoilUseRatio_type 
	"Return the type of attribute #overSoilUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">ressCoeff_unit 
	"Return the unit of attribute #ressCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">waterUseCoeff_unit 
	"Return the unit of attribute #waterUseCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">minYearlyGrazed_type 
	"Return the type of attribute #minYearlyGrazed.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">minPlotRessRate_unit 
	"Return the unit of attribute #minPlotRessRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overUseStepThreshold_type 
	"Return the type of attribute #overUseStepThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ressCoefRPtypeDeux_type 
	"Return the type of attribute #ressCoefRPtypeDeux.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">userInitCapital_unit 
	"Return the unit of attribute #userInitCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressCoefRPtypeUn_type 
	"Return the type of attribute #ressCoefRPtypeUn.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ressUseCoeff_type 
	"Return the type of attribute #ressUseCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscapeIIIRessValue_type 
	"Return the type of attribute #landscapeIIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overUseYearlyThreshold_type 
	"Return the type of attribute #overUseYearlyThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">RSUseCoeff_type 
	"Return the type of attribute #RSUseCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscapeIVRessValue_type 
	"Return the type of attribute #landscapeIVRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">minPlotRess_type 
	"Return the type of attribute #minPlotRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">overUseRessThreshold_unit 
	"Return the unit of attribute #overUseRessThreshold.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscapeIIRessValue_unit 
	"Return the unit of attribute #landscapeIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">livingStandard_unit 
	"Return the unit of attribute #livingStandard.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressCoefRPtypeDeux_unit 
	"Return the unit of attribute #ressCoefRPtypeDeux.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressCoefRPtypeUn_unit 
	"Return the unit of attribute #ressCoefRPtypeUn.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressOfTempRessProv_type 
	"Return the type of attribute #ressOfTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">waterUseCoeff_type 
	"Return the type of attribute #waterUseCoeff.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscapeIIIRessValue_unit 
	"Return the unit of attribute #landscapeIIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscapeIIRessValue_type 
	"Return the type of attribute #landscapeIIRessValue.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">ressOfTempRessProv_unit 
	"Return the unit of attribute #ressOfTempRessProv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">useCoeffRSTemp_type 
	"Return the type of attribute #useCoeffRSTemp.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Calibration class</class-id> <category>- default value</category>

<body package="WitUses">overUseStepThreshold_default 
	"Return the default value of attribute #overUseStepThreshold : 4.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^4</body>

<body package="WitUses">landscapeIRessValue_default 
	"Return the default value of attribute #landscapeIRessValue : 0.25.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.25</body>

<body package="WitUses">landscapeIVRessValue_default 
	"Return the default value of attribute #landscapeIVRessValue : 1.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1.6</body>

<body package="WitUses">overUseYearlyThreshold_default 
	"Return the default value of attribute #overUseYearlyThreshold : 4.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^4</body>

<body package="WitUses">overUseThreshold_default 
	"Return the default value of attribute #overUseThreshold : 9.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^9</body>

<body package="WitUses">ressOfSteadyRessProv_default 
	"Return the default value of attribute #ressOfSteadyRessProv : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">userInitCapital_default 
	"Return the default value of attribute #userInitCapital : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">overGrazedRessRatio_default 
	"Return the default value of attribute #overGrazedRessRatio : 0.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.6</body>

<body package="WitUses">minSeasPlotRess_default 
	"Return the default value of attribute #minSeasPlotRess : 0.33.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.33</body>

<body package="WitUses">overGrazedYearlyThreshold_default 
	"Return the default value of attribute #overGrazedYearlyThreshold : 4.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^4</body>

<body package="WitUses">overSoilUseRatio_default 
	"Return the default value of attribute #overSoilUseRatio : 0.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.6</body>

<body package="WitUses">ressUseCoeff_default 
	"Return the default value of attribute #ressUseCoeff : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">landscapeIIRessValue_default 
	"Return the default value of attribute #landscapeIIRessValue : 0.5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.5</body>

<body package="WitUses">ressOfTempRessProv_default 
	"Return the default value of attribute #ressOfTempRessProv : 1.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1.6</body>

<body package="WitUses">minYearlyGrazed_default 
	"Return the default value of attribute #minYearlyGrazed : 1.1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1.1</body>

<body package="WitUses">RSUseCoeff_default 
	"Return the default value of attribute #RSUseCoeff : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">overRessRsRatio_default 
	"Return the default value of attribute #overRessRsRatio : 0.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.6</body>

<body package="WitUses">overUseRessThreshold_default 
	"Return the default value of attribute #overUseRessThreshold : 0.5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.5</body>

<body package="WitUses">ressCoefRPtypeUn_default 
	"Return the default value of attribute #ressCoefRPtypeUn : 100.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^100</body>

<body package="WitUses">minPlotRess_default 
	"Return the default value of attribute #minPlotRess : 0.33.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.33</body>

<body package="WitUses">minPlotRessRate_default 
	"Return the default value of attribute #minPlotRessRate : 0.2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.2</body>

<body package="WitUses">ressCoefRPtypeDeux_default 
	"Return the default value of attribute #ressCoefRPtypeDeux : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ressCoeff_default 
	"Return the default value of attribute #ressCoeff : 0.7.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.7</body>

<body package="WitUses">landscapeIIIRessValue_default 
	"Return the default value of attribute #landscapeIIIRessValue : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">useCoeffRSTemp_default 
	"Return the default value of attribute #useCoeffRSTemp : 5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^5</body>

<body package="WitUses">overUseRessRatio_default 
	"Return the default value of attribute #overUseRessRatio : 0.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.6</body>

<body package="WitUses">livingStandard_default 
	"Return the default value of attribute #livingStandard : 2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^2</body>

<body package="WitUses">overSoilYearlyThreshold_default 
	"Return the default value of attribute #overSoilYearlyThreshold : 4.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^4</body>

<body package="WitUses">waterUseCoeff_default 
	"Return the default value of attribute #waterUseCoeff : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.User class</class-id> <category>+ parameter unit</category>

<body package="WitUses">myTerritory_type 
	"Return the type of attribute #myTerritory.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">allusers_type 
	"Return the type of attribute #allusers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">capital_type 
	"Return the type of attribute #capital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">myCurrentSPUs_type 
	"Return the type of attribute #myCurrentSPUs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">socialGroup_type 
	"Return the type of attribute #socialGroup.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">myLivestock_unit 
	"Return the unit of attribute #myLivestock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">yearlyTappedRessRS_Setlled_type 
	"Return the type of attribute #yearlyTappedRessRS_Setlled.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">availSPUsed_type 
	"Return the type of attribute #availSPUsed.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">allSeasonPlotUses_type 
	"Return the type of attribute #allSeasonPlotUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">yearlyGrazedStock_unit 
	"Return the unit of attribute #yearlyGrazedStock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">yearlyTappedRessSr_type 
	"Return the type of attribute #yearlyTappedRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">myHungryLivestock_type 
	"Return the type of attribute #myHungryLivestock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">seasonTappedRessRs_unit 
	"Return the unit of attribute #seasonTappedRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">capital_unit 
	"Return the unit of attribute #capital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">myLivestock_type 
	"Return the type of attribute #myLivestock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">myHungryLivestock_unit 
	"Return the unit of attribute #myHungryLivestock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">allSeasonPlots_type 
	"Return the type of attribute #allSeasonPlots.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">yearlyTappedRessRS_Mobile_unit 
	"Return the unit of attribute #yearlyTappedRessRS_Mobile.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">yearlyTappedRessSr_unit 
	"Return the unit of attribute #yearlyTappedRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">appliedPolicies_type 
	"Return the type of attribute #appliedPolicies.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">myPlotsCurrentSeason_type 
	"Return the type of attribute #myPlotsCurrentSeason.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">mySeasonPlotUses_type 
	"Return the type of attribute #mySeasonPlotUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">hotPlots_type 
	"Return the type of attribute #hotPlots.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">seasonTappedRessRs_type 
	"Return the type of attribute #seasonTappedRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">yearlyGrazedStock_type 
	"Return the type of attribute #yearlyGrazedStock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">allRulesImplementers_type 
	"Return the type of attribute #allRulesImplementers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">myAvailableSPUs_type 
	"Return the type of attribute #myAvailableSPUs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">yearlyTappedRess_r_type 
	"Return the type of attribute #yearlyTappedRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">allRessProviders_type 
	"Return the type of attribute #allRessProviders.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">testAvailSPU_type 
	"Return the type of attribute #testAvailSPU.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">yearlyStartingPlot_type 
	"Return the type of attribute #yearlyStartingPlot.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">rulesImplementers_type 
	"Return the type of attribute #rulesImplementers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">allSeasonPlotUses_unit 
	"Return the unit of attribute #allSeasonPlotUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">yearlyTappedRessRS_Mobile_type 
	"Return the type of attribute #yearlyTappedRessRS_Mobile.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">yearlyTappedRessRS_Setlled_unit 
	"Return the unit of attribute #yearlyTappedRessRS_Setlled.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">yearlyTappedRess_r_unit 
	"Return the unit of attribute #yearlyTappedRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User class</class-id> <category>- probes</category>

<body package="WitUses">markerOfProbe_capital
  ^ #none</body>

<body package="WitUses">markerColorOfProbe_capital
  ^ nil</body>

<body package="WitUses">colorOfProbe_capital
  ^ (ColorValue red: 1.0 green: 0.624954 blue: 0.250031)</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User class</class-id> <category>- default value</category>

<body package="WitUses">seasonTappedRessRS_Settled_default 
	"Return the default value of attribute #seasonTappedRessRS_Settled : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">yearlyStartingPlot_default 
	"Return the default value of attribute #yearlyStartingPlot : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">appliedPolicies_default 
	"Return the default value of attribute #appliedPolicies : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">capital_default 
	"Return the default value of attribute #capital : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">rulesImplementers_default 
	"Return the default value of attribute #rulesImplementers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">myAvailableSPUs_default 
	"Return the default value of attribute #myAvailableSPUs : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">mySeasonPlotUses_default 
	"Return the default value of attribute #mySeasonPlotUses : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">socialGroup_default 
	"Return the default value of attribute #socialGroup : ' '.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^' '</body>

<body package="WitUses">yearlyTappedRessRS_Settlled_default 
	"Return the default value of attribute #yearlyTappedRessRS_Settlled : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">availSPUsed_default 
	"Return the default value of attribute #availSPUsed : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">yearlyTappedRessRS_Setlled_default 
	"Return the default value of attribute #yearlyTappedRessRS_Setlled : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">yearlyTappedRess_r_default 
	"Return the default value of attribute #yearlyTappedRess_r : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">myPlotsCurrentSeason_default 
	"Return the default value of attribute #myPlotsCurrentSeason : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">seasonTappedRessRs_default 
	"Return the default value of attribute #seasonTappedRessRs : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">allSeasonPlotUses_default 
	"Return the default value of attribute #allSeasonPlotUses : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">allusers_default 
	"Return the default value of attribute #allusers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">myHungryLivestock_default 
	"Return the default value of attribute #myHungryLivestock : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">yearlyGrazedStock_default 
	"Return the default value of attribute #yearlyGrazedStock : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">allRulesImplementers_default 
	"Return the default value of attribute #allRulesImplementers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">yearlyTappedRessRS_Mobile_default 
	"Return the default value of attribute #yearlyTappedRessRS_Mobile : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">allRessProviders_default 
	"Return the default value of attribute #allRessProviders : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">myTerritory_default 
	"Return the default value of attribute #myTerritory : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">myLivestock_default 
	"Return the default value of attribute #myLivestock : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">yearlyTappedRessSr_default 
	"Return the default value of attribute #yearlyTappedRessSr : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">hotPlots_default 
	"Return the default value of attribute #hotPlots : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">myCurrentSPUs_default 
	"Return the default value of attribute #myCurrentSPUs : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">testAvailSPU_default 
	"Return the default value of attribute #testAvailSPU : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">seasonTappedRessRS_Mobile_default 
	"Return the default value of attribute #seasonTappedRessRS_Mobile : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">allSeasonPlots_default 
	"Return the default value of attribute #allSeasonPlots : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">yearlyTappedRessRs_default 
	"Return the default value of attribute #yearlyTappedRessRs : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">seasonTappedRess_r_default 
	"Return the default value of attribute #seasonTappedRess_r : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">seasonTappedRessSr_default 
	"Return the default value of attribute #seasonTappedRessSr : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">yearlyTappedRessRS_Settled_default 
	"Return the default value of attribute #yearlyTappedRessRS_Settled : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User class</class-id> <category>pov symbols</category>

<body package="WitUses">defaultColor 
"Returns the default color (Automatically defined by Cormas)." 
^(ColorValue red: 0.600049 green: 0.900012 blue: 0.0)</body>

<body package="WitUses">VectorialPOV_User
"Returns an array containing the settings of a Situated entity vectorial pov symbol in the following order:
(color red) (color green) (color blue) (lineColor red) (lineColor green) (lineColor blue) lineWidth sizeratio position (array of outline vertices) isDistortable." 
^#(0.600049 0.900012 0.0 0.0 0.0 0.0 1 1 #random #(4.45856e-7@-10.2 8.83346@-5.1 8.83346@5.1 -1.21634e-7@10.2 -8.83346@5.1 -8.83346@-5.1 3.44522e-6@-10.2) true)</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>accessing</category>

<body package="WitUses">myHungryLivestock: aSmallInteger 
	"Setter accessor of attribute myHungryLivestock	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myHungryLivestock = aSmallInteger ifTrue:[^nil].   "for optimization"
	myHungryLivestock := aSmallInteger. 
	self changed</body>

<body package="WitUses">seasonTappedRessRs: aSmallInteger 
	"Setter accessor of attribute seasonTappedRessRs	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	seasonTappedRessRs = aSmallInteger ifTrue:[^nil].   "for optimization"
	seasonTappedRessRs := aSmallInteger. 
	self changed</body>

<body package="WitUses">allSeasonPlotUses: aOrderedCollection 
	"Setter accessor of attribute allSeasonPlotUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allSeasonPlotUses := aOrderedCollection</body>

<body package="WitUses">myHungryLivestock
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myHungryLivestock ifNil:[myHungryLivestock := 0]</body>

<body package="WitUses">allSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allSeasonPlotUses ifNil:[allSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">myCurrentSPUs: aOrderedCollection 
	"Setter accessor of attribute myCurrentSPUs	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myCurrentSPUs := aOrderedCollection</body>

<body package="WitUses">socialGroup: aByteString 
	"Setter accessor of attribute socialGroup	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	socialGroup := aByteString</body>

<body package="WitUses">appliedRules: aOrderedCollection 
	"Setter accessor of attribute appliedPolicies	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	appliedRules = aOrderedCollection ifTrue:[^nil].   "for optimization"
	appliedRules := aOrderedCollection. 
	self changed</body>

<body package="WitUses">myTerritory: aOrderedCollection 
	"Setter accessor of attribute myTerritory	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myTerritory := aOrderedCollection</body>

<body package="WitUses">seasonTappedRessSr
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^seasonTappedRessSr ifNil:[seasonTappedRessSr := 0]</body>

<body package="WitUses">yearlyTappedRessRs: aSmallInteger 
	"Setter accessor of attribute yearlyGrazedStock	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	yearlyTappedRessRs = aSmallInteger ifTrue:[^nil].   "for optimization"
	yearlyTappedRessRs := aSmallInteger. 
	self changed</body>

<body package="WitUses">hotPlots
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^hotPlots ifNil:[hotPlots := OrderedCollection new]</body>

<body package="WitUses">seasonTappedRessRS_Mobile
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^seasonTappedRessRS_Mobile ifNil:[seasonTappedRessRS_Mobile := 0]</body>

<body package="WitUses">hotPlots: aOrderedCollection 
	"Setter accessor of attribute hotPlots	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	hotPlots = aOrderedCollection ifTrue:[^nil].   "for optimization"
	hotPlots := aOrderedCollection. 
	self changed</body>

<body package="WitUses">yearlyStartingPlot
	"Getter accessor without initial value "
	^yearlyStartingPlot</body>

<body package="WitUses">allRulesImplementers: aOrderedCollection 
	"Setter accessor of attribute allRulesImplementers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allRulesImplementers = aOrderedCollection ifTrue:[^nil].   "for optimization"
	allRulesImplementers := aOrderedCollection. 
	self changed</body>

<body package="WitUses">myLivestock
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myLivestock ifNil:[myLivestock := 0]</body>

<body package="WitUses">appliedRules
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^appliedRules ifNil:[appliedRules := OrderedCollection new]</body>

<body package="WitUses">seasonTappedRess_r
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^seasonTappedRess_r ifNil:[seasonTappedRess_r := 0]</body>

<body package="WitUses">seasonTappedRess_r: aSmallInteger 
	"Setter accessor of attribute yearlyTappedRess_r	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	seasonTappedRess_r = aSmallInteger ifTrue:[^nil].   "for optimization"
	seasonTappedRess_r := aSmallInteger. 
	self changed</body>

<body package="WitUses">myCurrentSPUs
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myCurrentSPUs ifNil:[myCurrentSPUs := OrderedCollection new]</body>

<body package="WitUses">allRessProviders: aOrderedCollection 
	"Setter accessor of attribute allRessProviders	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allRessProviders := aOrderedCollection</body>

<body package="WitUses">myLivestock: aSmallInteger 
	"Setter accessor of attribute myLivestock	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myLivestock := aSmallInteger</body>

<body package="WitUses">allRulesImplementers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allRulesImplementers ifNil:[allRulesImplementers := OrderedCollection new]</body>

<body package="WitUses">allRessProviders
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allRessProviders ifNil:[allRessProviders := OrderedCollection new]</body>

<body package="WitUses">myTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myTerritory ifNil:[myTerritory := OrderedCollection new]</body>

<body package="WitUses">seasonTappedRessRs
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^seasonTappedRessRs ifNil:[seasonTappedRessRs := 0]</body>

<body package="WitUses">seasonTappedRessRS_Settled: aSmallInteger 
	"Setter accessor of attribute yearlyTappedRessRS_Setlled	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	seasonTappedRessRS_Settled = aSmallInteger ifTrue:[^nil].   "for optimization"
	seasonTappedRessRS_Settled := aSmallInteger. 
	self changed</body>

<body package="WitUses">capital: aSmallInteger 
	"Setter accessor of attribute capital	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	capital := aSmallInteger</body>

<body package="WitUses">seasonTappedRessRS_Mobile: aSmallInteger 
	"Setter accessor of attribute yearlyTappedRessRS_Mobile	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	seasonTappedRessRS_Mobile = aSmallInteger ifTrue:[^nil].   "for optimization"
	seasonTappedRessRS_Mobile := aSmallInteger. 
	self changed</body>

<body package="WitUses">allUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allUsers ifNil:[allUsers := OrderedCollection new]</body>

<body package="WitUses">seasonTappedRessSr: aSmallInteger 
	"Setter accessor of attribute yearlyTappedRessSr	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	seasonTappedRessSr = aSmallInteger ifTrue:[^nil].   "for optimization"
	seasonTappedRessSr := aSmallInteger. 
	self changed</body>

<body package="WitUses">socialGroup
	"Getter accessor with initial value = ' '. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^socialGroup ifNil:[socialGroup := ' ']</body>

<body package="WitUses">seasonTappedRessRS_Settled
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^seasonTappedRessRS_Settled ifNil:[seasonTappedRessRS_Settled := 0]</body>

<body package="WitUses">yearlyTappedRessRs
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^yearlyTappedRessRs ifNil:[yearlyTappedRessRs := 0]</body>

<body package="WitUses">yearlyStartingPlot: aUndefinedObject 
	"Setter accessor of attribute yearlyStartingPlot	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	yearlyStartingPlot = aUndefinedObject ifTrue:[^nil].   "for optimization"
	yearlyStartingPlot := aUndefinedObject. 
	self changed</body>

<body package="WitUses">allUsers: aOrderedCollection 
	"Setter accessor of attribute allusers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allUsers = aOrderedCollection ifTrue:[^nil].   "for optimization"
	allUsers := aOrderedCollection. 
	self changed</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>implementingUses</category>

<body package="WitUses">implementUses

"Je met en oeuvre, dans cet lordre-là, mes activités Settled (agriculture), puis Mobile (élevage), puis Picking (cueillette)"

"Je fais le point sur mon cheptel (en début d'année, si je peux aquérir de nouveaux, et à chaque saison combien de troupeaux j'ai à nourrir)"
self assessMobileStock.
"Je remet ensuite à zéro les ressTapped de la saison précédente"
self seasonTappedRessRS_Settled:0. self seasonTappedRessSr:0.
self seasonTappedRessRS_Mobile:0. self seasonTappedRessRs:0.
self seasonTappedRess_r:0.

"J'essaie ensuite en priorité si j'en ai les moyens d'installer une activité agricole (s'il n'y a pas d'investissement requis -requiredCapital-, cela doit marcher aussi avec un capital d'User négatif)"
(SettledUse requiredCapital=0) ifTrue:[self launchingSettling] ifFalse:[(self capital &gt;= SettledUse requiredCapital) ifTrue:[self launchingSettling]].
"puis si j'en ai les moyens, je m'occupe de mes troupeaux"
(MobileUse requiredCapital=0) ifTrue:[self launchingMobile] ifFalse:[(self capital &gt;= MobileUse requiredCapital) ifTrue:[self launchingMobile]].
"puis si j'en ai les moyens, je m'occupe de mes activités de picking"
(PickUse requiredCapital=0) ifTrue:[self launchingPicking] ifFalse:[(self capital &gt;= PickUse requiredCapital) ifTrue:[self launchingPicking]].
"Puis à la fin de chaque année, j'évalue l'état de mes troupeaux et leur productivité" 
((WitUses timeStep\\4)=0) ifTrue: [self assessYearlyMobileIncomes].</body>

<body package="WitUses">locatedEntities: aClass verifying: aBlock withinRadius: anInteger around: 
aSpatialEntity

	|aCollection|
	aSpatialEntity ifNil: [^#()].
	aCollection := OrderedCollection new.
	(aSpatialEntity recursiveNeighbourhood: anInteger) do: [:aSE | aCollection addAll: (aSE occupantsType: aClass)].
	(aCollection includes: self) ifTrue: [aCollection remove: self].
	^aCollection select: [:aSE | aBlock value: aSE]</body>

<body package="WitUses">addingSameGroupSpusAs: myAvailableSeasonPlots for: myUse

| myGroupSpus |

" Les spus de mon groupe deviennent aussi mes spus"
myGroupSpus:= IndexedSet new.
self allSeasonPlotUses do: [:spu | ((self allUsers select: [:u | u socialGroup = self socialGroup]) contains: [:u | spu currentUsers includes: u]) ifTrue: [myGroupSpus add: spu]].
myGroupSpus removeAllSuchThat: [:spu | spu use ~= myUse].
myAvailableSeasonPlots addAll: myGroupSpus.
"Je sélectionne les SPU qui me sont déjà attribués (en fonction de la règle appliquée)"
self sortingSPUs: myAvailableSeasonPlots for: myUse.
"en y appliquant les conditions spécifiques de ressource à l'usage"
self applyingWaterSoilConditionsFor: myUse to: myAvailableSeasonPlots.
"Si j'ai des spus adéquats, je sélectionne puis exploite le plot le moins éloigné (en incluant mon patch), aux conditions de l'usage considéré"
(myAvailableSeasonPlots isEmpty) ifFalse: [self selectingNearestPlotFrom: myAvailableSeasonPlots for: myUse]
"et s'il n'y en a pas, je me tourne vers les SPU qui ne m'ont pas encore été attribués, en commencant par regarder les spu des parcelles adjacentes à celle où je me trouve"
ifTrue: [self exploreNeighbour:myAvailableSeasonPlots for: myUse]</body>

<body package="WitUses">exploreNeighbour:myAvailableSeasonPlots for: myUse

"D'abord, je sélectionne les SPU des voisins de ma cellule qui concernent la même activité"
self allSeasonPlotUses do: [:spu | ((self patch neighbourhoodAndSelf) contains: [:p | p = spu hostedPlot]) ifTrue: [myAvailableSeasonPlots add: spu]]. myAvailableSeasonPlots removeAllSuchThat: [:spu | spu use ~= myUse].
" Parmi ces SPU, je sélectionne les SPUS disponibles concernant la saison considérée et l'usage considéré"
self sortingSPUs: myAvailableSeasonPlots for: myUse.
"en y appliquant les conditions spécifiques de ressource à l'usage"
self applyingWaterSoilConditionsFor: myUse to: myAvailableSeasonPlots.
"s'il y en a, je sélectionne au hasard l'un des plots concernés, et je l'exploite aux conditions de l'usage considéré"
(myAvailableSeasonPlots isEmpty) ifFalse: [self selectingOnePlotFrom: myAvailableSeasonPlots for: myUse]
"et s'il n'y en a pas, je me tourne d'abord vers les SPU de mon groupe social, puis si pas possible je regarde n'importe où dans le territoire les SPU libres et qui correspondent à des parcelles aux ressources suffisantes"
ifTrue: [
(self appliedRules includes: 'socialNetwork') ifTrue: [self addingSameGroupSpusAs:myAvailableSeasonPlots for: myUse]
ifFalse: [self exploreRandom:myAvailableSeasonPlots for: myUse]].</body>

<body package="WitUses">allocatingMyOwnSpusFor: myUse

"D'abord, je regarde d'abord parmi les spus qui me sont déjà attribués"

| myOwnSpus myAvailableSeasonPlots |
myAvailableSeasonPlots:= IndexedSet new.

myOwnSpus:= (self allSeasonPlotUses select: [:spu | spu use = myUse]) select: [:spu | spu currentUsers includes: self].
myAvailableSeasonPlots addAll:myOwnSpus.
self sortingSPUs: myAvailableSeasonPlots for: myUse.
"en y appliquant les conditions spécifiques de ressource à l'usage"
self applyingWaterSoilConditionsFor: myUse to: myAvailableSeasonPlots.
"Si j'ai des spus adéquats, je sélectionne puis exploite le plot le moins éloigné (en incluant mon patch), aux conditions de l'usage considéré"
(myAvailableSeasonPlots isEmpty) ifFalse: [self selectingNearestPlotFrom: myAvailableSeasonPlots for: myUse]
"et s'il n'y en a pas, je me tourne d'abord vers les cellules voisines de la mienne"
ifTrue: [self exploreNeighbour:myAvailableSeasonPlots for: myUse].</body>

<body package="WitUses">exploreRandom:myAvailableSeasonPlots for: myUse

" Je sélectionne les SPUS de tout le territoire qui sont disponibles concernant l'usage considérés"
myAvailableSeasonPlots addAll: (self allSeasonPlotUses select: [: spu | spu use=myUse]).
"Je sélectionne les SPUS disponibles concernant la saison considérée"
self sortingSPUs: myAvailableSeasonPlots for: myUse.
"j'y applique les conditions de ressources spécifiques à Settleduse"
self sortingSPUs: myAvailableSeasonPlots for: myUse.
"en y appliquant les conditions spécifiques de ressource à l'usage"
self applyingWaterSoilConditionsFor: myUse to: myAvailableSeasonPlots.
"s'il y en a, je sélectionne puis exploite le plot le moins éloigné (en incluant mon patch), aux conditions de l'usage considéré"
(myAvailableSeasonPlots isEmpty) ifFalse: [self selectingNearestPlotFrom: myAvailableSeasonPlots for: myUse]
"et s'il n'y en a pas, je regarde les SPU dont les parcelles ont des ressources en sol suffisantes et qui sont contigues d'un point d'eau assez approvisionné"
ifTrue: [(myUse~='Picking') ifTrue: [self exploreProvider: myAvailableSeasonPlots for: myUse]]</body>

<body package="WitUses">exploreProvider: myAvailableSeasonPlots for: myUse
	
| myOwnSPUs |

"je sélectionne les SPUS des parcelles se situant dans un rayon accessible (MobileUse myRadius) autour d'un point d'eau approvisionné"
self sortingSPUClosedFromRessProv:myAvailableSeasonPlots for: myUse.
".j'y sélectionne les SPUS disponibles concernant la saison considérée et l'usage considéré"
self sortingSPUs: myAvailableSeasonPlots for: myUse.
"Parmi ces SPU, je regarde s'il n'y en a pas un qui m'ait déjà attribué (car je ne les avais sélectionnés auparavant que s'ils avaient des ressources en eau)"
myOwnSPUs:= myAvailableSeasonPlots select: [:sp | sp currentUsers includes: self].
(myOwnSPUs isNotEmpty) ifTrue: [myAvailableSeasonPlots removeAll. myAvailableSeasonPlots addAll: myOwnSPUs.
"s'il y en a, je sélectionne puis exploite le plot le moins éloigné (en incluant mon patch), aux conditions de l'usage considéré"
self selectingNearestPlotFrom: myAvailableSeasonPlots for: myUse]
"s'il n'y en pas de plots qui me sont déjà attribués et qui sont exploitables, si j'ai d'autres SPU dispo je sélectionne puis exploite le plot le moins éloigné (en incluant mon patch), aux conditions de l'usage considéré"
ifFalse: [(myAvailableSeasonPlots isNotEmpty) ifTrue:[self selectingNearestPlotFrom: myAvailableSeasonPlots for: myUse]]
"s'il n'y a pas de SPU adéquats contigus à un point d'eau approvisionné,  je passe à l'activité suivante"</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>scanningForSpus</category>

<body package="WitUses">applyingWaterSoilConditionsFor: myUse to: myAvailableSeasonPlots

"Si agriculture, les drotis d'accès aux plots qui n'ont pas assez de ressources en eau sont exclus"
(myUse='Settled') ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu hostedPlot remainRessRS &lt; SettledUse needRS]].
"Si élevage, seulement en saison sèche, je vérifie qu'il y a assez d'eau sur le plot"
(myUse='Mobile') ifTrue: [(((WitUses timeStep\\4) =3) or: [((WitUses timeStep\\4) =0)]) ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu hostedPlot remainRessRS &lt; MobileUse needRS]]].</body>

<body package="WitUses">sortingSPUs: myAvailableSeasonPlots for: myUse

"D'abord, je sélectionne les spus correspondant à la saison"
((WitUses timeStep\\4) =1) ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu season ~= '1']].
((WitUses timeStep\\4) =2) ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu season ~= '2']].
((WitUses timeStep\\4) =3) ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu season ~= '3']].
((WitUses timeStep\\4) =0) ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu season ~= '4']].
"j'y applique les contraintes de sélection dues à la politique foncière appliquée (par overall RulesImplementer)"
self appliedLandTenureRules: myAvailableSeasonPlots for: myUse.
"et sur ces SPU, je sélectionne ceux dont les plots ont encore les ressources correspondant à mon usage (hormis l'eau des sols, de façon à ne pas évacuer les parcelles autour points d'eau)"
self selectingSPUsWithRess:myAvailableSeasonPlots for: myUse.
"en cas de situation de crise exceptionnelle, on y rajoute les plots aux ressources clés (mises alors en accès libre)"
(self hotPlots isEmpty) ifFalse: [self selectingHotPlotswithRSfor: myUse. self addingHotPlotsSPUsIn:myAvailableSeasonPlots for: myUse].</body>

<body package="WitUses">selectingSPUsWithRess:myAvailableSeasonPlots for: myUse

"je sélectionne les plots qui ont encore les ressources correspondant à mon usage (hormis l'eau des sols, de façon à ne pas évacuer les parcelles autour points d'eau)"

(myUse='Settled') ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu hostedPlot remainRessSr &lt; SettledUse ressTapping]].
(myUse='Mobile') ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu hostedPlot remainRessRs = 0]].
(myUse='Picking') ifTrue: [myAvailableSeasonPlots removeAllSuchThat: [:spu | spu hostedPlot remainRess_r = 0]].</body>

<body package="WitUses">sortingSPUClosedFromRessProv: myAvailableSeasonPlots for: myUse

| useablePlots myNeedRS myRadius |

useablePlots:= IndexedSet new.
(myUse= 'Settled') ifTrue: [myNeedRS:= (2*SettledUse needRS). myRadius:= SettledUse myRadius].
(myUse= 'Mobile') ifTrue: [myNeedRS:= MobileUse needRS. myRadius:= MobileUse myRadius].
"Je repère les plots possédant ayant à la fois des ressources pour l'usage et un point d'eau encore suffisamment approvisionné en RS "
(self allRessProviders select: [:rp | rp remainRess &gt;= myNeedRS]) do: [:rp | useablePlots := rp patch recursiveNeighbourhood: myRadius].
(myUse= 'Settled') ifTrue: [useablePlots removeAllSuchThat: [:p | p remainRessSr &lt; SettledUse ressTapping]].
(myUse= 'Mobile') ifTrue: [useablePlots removeAllSuchThat: [:p | p remainRessRs &lt; MobileUse ressTapping]].
"et je récupère ceux de leurs SPU qui concerne l'usage considéré"
(self allSeasonPlotUses select: [:spu | spu use=myUse]) do: [:spu | (useablePlots contains: [:p | p = spu hostedPlot]) ifTrue: [myAvailableSeasonPlots add: spu]].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>selectingTheRightPlot</category>

<body package="WitUses">selectingOnePlotFrom: myAvailableSeasonPlots for: myUse

| mySelectedSeasonPlot myPlot useablePlots selectedUse |

"Sélection puis exploitation du plot disponible, aux conditions de l'usage considéré"
(myUse = 'Settled') ifTrue: [selectedUse:=SettledUse].(myUse = 'Mobile') ifTrue: [selectedUse:=MobileUse].(myUse = 'Picking') ifTrue: [selectedUse:=PickUse]. (myUse = 'Intensive') ifTrue: [selectedUse:=IntensiveUse].
"je sélectionne au hasard l''un des plots des SPUs disponibles, en commençant par mon patch"
useablePlots:= OrderedCollection new.
self allSeasonPlotUses do: [:spu | (myAvailableSeasonPlots includes: spu) ifTrue: [useablePlots add: spu hostedPlot]].
(useablePlots includes:self patch) ifTrue: [myPlot:= self patch] ifFalse: [myPlot:= Cormas selectRandomlyFrom: useablePlots].
"je sélectionne le spu correspondant, je m'y inscris et j'enregistre pour moi que je vais l'utiliser"
myAvailableSeasonPlots do: [:spu | (spu hostedPlot= myPlot) ifTrue: [mySelectedSeasonPlot :=spu ]]. mySelectedSeasonPlot currentUsers add: self. self myCurrentSPUs add:mySelectedSeasonPlot.
"j'utilise le capital nécessaire pour mettre en oeuvre l'usage de la parcelle"
self capital: self capital - selectedUse requiredCapital.
"j'en consomme les ressources comme indiqué dans la classe de l'usage concerné (en enregistrant les ressources consommées (pour indicateurs suivi)"
(myUse= 'Settled') ifTrue:[self moveTo: myPlot. self usingSettledRess: myPlot].
(myUse= 'Mobile') ifTrue:[self usingMobileRess: myPlot].
(myUse= 'Picking') ifTrue:[self usingPickingRess: myPlot].
"en fin d'année, je retourne au plot de départ de l'année, comme étant ma résidence habituelle"
((WitUses timeStep\\4)=0) ifTrue:[self moveTo: self yearlyStartingPlot].</body>

<body package="WitUses">selectingNearestPlotFrom: myAvailableSeasonPlots for: myUse

| useablePlots myPlot mySelectedSeasonPlot selectedUse |
(myUse = 'Settled') ifTrue: [selectedUse:=SettledUse].(myUse = 'Mobile') ifTrue: [selectedUse:=MobileUse].(myUse = 'Picking') ifTrue: [selectedUse:=PickUse]. (myUse = 'Intensive') ifTrue: [selectedUse:=IntensiveUse].

"Sélection puis exploitation du plus proche plot disponible, aux conditions de l'usage considéré"

"je sélectionne le plot le plus proche d'un des SPUS disponibles"
useablePlots:= OrderedCollection new.
self allSeasonPlotUses do: [:spu |  (myAvailableSeasonPlots includes: spu) ifTrue: [useablePlots add: spu hostedPlot]].
(useablePlots includes:self patch) ifTrue: [myPlot:= self patch] ifFalse: [myPlot:= (self patch nearestNeighborVerifying: [:p | useablePlots includes: p] withinRadius: 12)].
"je sélectionne le spu correspondant, je m'y inscris et j'enregistre pour moi que je vais l'utiliser"
myAvailableSeasonPlots do: [:spu | (spu hostedPlot= myPlot) ifTrue: [mySelectedSeasonPlot :=spu ]]. mySelectedSeasonPlot currentUsers add: self. self myCurrentSPUs add:mySelectedSeasonPlot.
"j'utilise le capital nécessaire pour mettre en oeuvre l'usage de la parcelle"
self capital: self capital - selectedUse requiredCapital.
"j'en consomme les ressources comme indiqué dans la classe de l'usage concerné (en enregistrant les ressources consommées pour indicateurs suivi)"
(myUse= 'Settled') ifTrue:[self moveTo: myPlot. self usingSettledRess: myPlot].
(myUse= 'Mobile') ifTrue:[self usingMobileRess: myPlot].
(myUse= 'Picking') ifTrue:[self usingPickingRess: myPlot].
"en fin d'année, je retourne au plot de départ de l'année, comme étant ma résidence habituelle"
((WitUses timeStep\\4)=0) ifTrue:[self moveTo: self yearlyStartingPlot].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>rulesConstraints</category>

<body package="WitUses">allocateSameGroupSpus

| myGroup myGroupSpus |

myGroup:= self allUsers select: [:u | u socialGroup = self socialGroup].
myGroupSpus:= self allSeasonPlotUses select: [:spu | myGroup contains: spu currentUsers].
(self allSeasonPlotUses select: [:spu | myGroup contains: spu currentUsers]) do: [:spu | spu currentUsers add: self].</body>

<body package="WitUses">addingHotPlotsSPUsIn:myAvailableSeasonPlots for: myUse

| hotPlotSpus allocatedSettledSpus myPlotsRP interestingRessProviders |
hotPlotSpus:= OrderedCollection new.

"Parmi les hotPlots proposés par RulesImplementer, je sélectionne les droits d'accès à ces plots (spus) concernant la saison en cours"
self allSeasonPlotUses do: [:spu | (self hotPlots includes: spu hostedPlot) ifTrue: [
((WitUses timeStep\\4) =1) ifTrue: [(spu season='1') ifTrue: [hotPlotSpus add: spu]].
((WitUses timeStep\\4) =2) ifTrue: [(spu season='2') ifTrue: [hotPlotSpus add: spu]].
((WitUses timeStep\\4) =3) ifTrue: [(spu season='3') ifTrue: [hotPlotSpus add: spu]].
((WitUses timeStep\\4) =0) ifTrue: [(spu season='4') ifTrue: [hotPlotSpus add: spu]]
]].
"Parmi ces SPU j'enlève ceux qui (à la même saison : voir ci-dessus) sont déjà attribués pour l'agriculture (settled), en préservant ceux qui me sont attribués pour Settled"
allocatedSettledSpus:= IndexedSet new. self allSeasonPlotUses do: [:spu | ((spu currentUsers includes: self) and: [spu use = 'Settled']) ifTrue: [allocatedSettledSpus add: spu]. hotPlotSpus removeAllSuchThat: [:sp | ((sp currentUsers isNotEmpty) and: [sp use = 'Settled'])]].

(myUse='Settled') ifTrue: [
"mais si je suis en SettledUse, je conserve ceux qui me sont attribués pour cette activité, dans la mesure où ces spus concernent des plots aux ressources suffisantes"
myPlotsRP:= self allRessProviders select: [:rp | rp patch = self patch]. (myPlotsRP isEmpty) ifTrue: [allocatedSettledSpus removeAllSuchThat: [:spu | spu hostedPlot remainRessRS &lt; SettledUse needRS]. hotPlotSpus addAll: allocatedSettledSpus].

(myUse='Mobile') ifTrue: [
(((WitUses timeStep\\4) =3) or: [((WitUses timeStep\\4) =0)]) ifTrue: [
myPlotsRP := self patch recursiveNeighbourhood: MobileUse myRadius. interestingRessProviders := OrderedCollection new. myPlotsRP do: [:p | interestingRessProviders addAll: ((p occupantsType: RessProvider) select: [:rp | rp remainRess&gt;= MobileUse needRS])]].
"en saison sèche, si pas de ressprovider accessible, je vérifie qu'il y a assez d'eau sur le plot"
(interestingRessProviders isEmpty) ifTrue: [hotPlotSpus removeAllSuchThat: [:spu | spu hostedPlot remainRessRS &lt; MobileUse needRS]]]].

myAvailableSeasonPlots addAll: hotPlotSpus.</body>

<body package="WitUses">appliedLandTenureRules: myAvailableSeasonPlots for: myUse

| allocatedSettledSpus availableSpus |

"J'appliquel a règle environnementale choisie : j'enlève les SPU en défens"
myAvailableSeasonPlots removeAllSuchThat: [:spu | spu currentUsers contains: [:u | u isKindOf: RulesImplementer]].

"Puis j'applique la règle foncière choisie"

((self appliedRules includes: 'firstUser') or:[self appliedRules includes: 'lastUser']) ifTrue: [
"Parmi ces SPU j'enlève ceux qui (à la même saison : voir méthodes de sélection avant celle-ci) sont déjà attribués pour l'agriculture (settled)"
allocatedSettledSpus := myAvailableSeasonPlots select: [:spu | ((spu currentUsers includes: self) and: [spu use = 'Settled'])].
myAvailableSeasonPlots removeAllSuchThat: [:spu | ((spu currentUsers isNotEmpty) and: [spu use = 'Settled'])].
"mais si je suis en SettledUse, je conserve ceux qui me sont attribués pour cette activité"
(myUse='Settled') ifTrue: [myAvailableSeasonPlots addAll: allocatedSettledSpus]].

(self appliedRules includes: 'onePlotOneUser') ifTrue: [
"Parmi ces SPU, je sélectionne les SPUS qui me sont attribués et ceux qui sont encore libres"
availableSpus := myAvailableSeasonPlots select: [:spu | (spu currentUsers includes: self) or: [spu currentUsers isEmpty]]. myAvailableSeasonPlots removeAll. myAvailableSeasonPlots addAll: availableSpus].</body>

<body package="WitUses">selectingHotPlotswithRSfor: myUse

| interestingRessProviders |

interestingRessProviders:= IndexedSet new.

(myUse = 'Settled') ifTrue:[
(self hotPlots select: [:p | p remainRessRS &lt; SettledUse needRS]) do: [:p | 
interestingRessProviders := self locatedEntities: RessProvider verifying: [:rp | rp remainRess&gt;= (2*SettledUse needRS)] withinRadius: SettledUse myRadius around: p. (interestingRessProviders isEmpty) ifTrue: [self hotPlots remove:p]]].

(myUse = 'Mobile') ifTrue:[
(((WitUses timeStep\\4) =3) or: [((WitUses timeStep\\4) =0)]) ifTrue: [
(self hotPlots select: [:p | p remainRessRS &lt; MobileUse needRS]) do: [:p | 
interestingRessProviders := self locatedEntities: RessProvider verifying: [:rp | rp remainRess&gt;= MobileUse needRS] withinRadius: MobileUse myRadius around: p. (interestingRessProviders isEmpty) ifTrue: [self hotPlots remove:p]]]].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>initialize-release</category>

<body package="WitUses">initSocialGroup

self socialGroup: (Cormas selectRandomlyFrom: self availableGroups).</body>

<body package="WitUses">initCapital

self capital: Calibration userInitCapital.</body>

<body package="WitUses">availableGroups
 ^ #('A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P')</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>settling</category>

<body package="WitUses">usingSettledRess: myPlot

| mySelectedRessProvider |

"j'enregistre le plot de départ de l'année, comme étant ma résidence habituelle"
((WitUses timeStep\\4)=1) ifTrue:[self yearlyStartingPlot: self patch].

"J'enregistre le capital obtenu par la mise en oeuvre de l'usage"
(myPlot remainRessSr &gt;= SettledUse ressTapping)
ifTrue: [self capital: self capital + (SettledUse incomesRate*SettledUse ressTapping).
"Je consomme le nombre d'unités en sol de la parcelle comme indiqué dans les paramètres de la classe Settled (en enregistrant les ressources consommées (pour indicateurs suivi) et s'il y en a moins, je prends juste les unités en sol qui y sont"
self seasonTappedRessSr: SettledUse ressTapping. myPlot remainRessSr: (myPlot remainRessSr - SettledUse ressTapping)]
ifFalse: [self capital: self capital + (SettledUse incomesRate*myPlot remainRessSr). self seasonTappedRessSr: myPlot remainRessSr. myPlot remainRessSr: 0].

"j'en consomme l'eau comme indiqué dans les paramètres de la classe Settled (en enregistrant les ressources consommées (pour indicateurs suivi)"
(myPlot remainRessRS &gt;= SettledUse needRS) ifTrue: [self seasonTappedRessRS_Settled: SettledUse needRS. myPlot remainRessRS: (myPlot remainRessRS - SettledUse needRS)]
"s'il n'y a pas assez d'eau dans le sol, je consomme l'eau du Ressprov situé sur la cellule"
ifFalse:[
mySelectedRessProvider:= (self allRessProviders select: [:rp | rp patch = self patch]) last. self seasonTappedRessRS_Settled: 2*SettledUse needRS. mySelectedRessProvider remainRess: (mySelectedRessProvider remainRess - 2*SettledUse needRS)].</body>

<body package="WitUses">launchingSettling
	| myUse | 

myUse:= 'Settled'.

self allocatingMyOwnSpusFor: myUse.</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>mobile</category>

<body package="WitUses">launchingMobile

	| myUse |

myUse:= 'Mobile'.

"Je mets l'activité en route pour les troupeaux à nourrir"
(self myHungryLivestock &gt; 0) ifTrue: [(self myHungryLivestock) timesRepeat: [self allocatingMyOwnSpusFor: myUse]].</body>

<body package="WitUses">assessMobileStock

"En début d'année, si je peux, j'acquiers un troupeau de plus"

((WitUses timeStep\\4)=1) ifTrue: [(self capital &gt;=MobileUse requiredInvest) ifTrue:[
(self myLivestock &lt; 12) ifTrue: [self capital: self capital - MobileUse requiredInvest. self myLivestock: self myLivestock + 1]]].
"En début de chaque saison, j'enregistre tous les troupeaux que je dois nourrir"
self myHungryLivestock: self myLivestock.</body>

<body package="WitUses">assessYearlyMobileIncomes

"Enregistre le déficit de productivité de mes troupeaux sur l'année (pas assez nourris), et le bénéfice obtenu avec les troupeaux"
self decreasingLivestock.
self capital: self capital + (self yearlyTappedRessRs*MobileUse incomesRate).
self yearlyTappedRessRs: 0.</body>

<body package="WitUses">decreasingLivestock

| maxTappedRess |

maxTappedRess:= MobileUse ressTapping * 4.

"j'enregistre le déficit de productivité de mes troupeaux (pas assez nourris)"
[self yearlyTappedRessRs &lt; (self myLivestock * MobileUse coeffMinYearlyTappRess * maxTappedRess)] whileTrue: [(self myLivestock - 1 &gt;=0) ifTrue: [self myLivestock: self myLivestock - 1]].</body>

<body package="WitUses">usingMobileRess: myPlot

| mySelectedRessProvider plotsAround interestingRessProviders|

interestingRessProviders := IndexedSet new. 

"j'enregistre que ce troupeau est maintenant nourri pour cette saison"
self myHungryLivestock: self myHungryLivestock - 1.

"Je consomme le nombre d'unités en herbe de la parcelle qui correspond à l'usage (voir attribut de la classe MobileUse) et s'il y en a moins, je prends juste les unités en herbe qui restent. ATTENTION POUR MOBILE USE, LE GAIN EN CAPITAL EST CALCULE SUR LA PRODUCTIVITE DES TROUPEAUX, PAS ICI"
(myPlot remainRessRs &gt;= MobileUse ressTapping)
ifTrue: [self seasonTappedRessRs: MobileUse ressTapping. myPlot remainRessRs: (myPlot remainRessRs - MobileUse ressTapping)]
ifFalse: [self seasonTappedRessRs: myPlot remainRessRs. myPlot remainRessRs: 0].
"et j'enregistre la ressource consommée pour évaluer en fin d'année si mes troupeaux ont été bien nourris"
self yearlyTappedRessRs: self yearlyTappedRessRs +self seasonTappedRessRs.

"Si l'on est en saison sèche 3 et 4, j'en consomme l'eau comme indiqué dans les paramètres pour un usage Mobile, et j'enregistre ce que le troupeau a réussi à consommer pour cette saison"
(((WitUses timeStep\\4) =3) or: [((WitUses timeStep\\4) =0)]) ifTrue: [
(myPlot remainRessRS &gt;= MobileUse needRS) ifTrue: [self seasonTappedRessRS_Mobile: self seasonTappedRessRS_Mobile + MobileUse needRS. myPlot remainRessRS: myPlot remainRessRS - MobileUse needRS]
"s'il n'y a pas assez d'eau dans la parcelle, je recherche le point d'eau à proximité"
											ifFalse: [plotsAround := myPlot recursiveNeighbourhood: MobileUse myRadius. plotsAround do: [:p | interestingRessProviders addAll: ((p occupantsType: RessProvider) select: [:rp | rp remainRess&gt;= MobileUse needRS])].
mySelectedRessProvider:= interestingRessProviders last. self seasonTappedRessRS_Mobile: self seasonTappedRessRS_Mobile + mySelectedRessProvider remainRess. mySelectedRessProvider remainRess: mySelectedRessProvider remainRess - MobileUse needRS]
													].

"je vérifie s'il me reste après cela encore des troupeaux à nourrir, et si c'est le cas, je recommence le processus de recherche de parcelle"
((self capital &gt;= MobileUse requiredCapital) and:[self myHungryLivestock &gt; 0]) ifTrue: [self launchingMobile]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>probes</category>

<body package="WitUses">capital
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^capital ifNil:[capital := 1]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.User</class-id> <category>picking</category>

<body package="WitUses">usingPickingRess: myPlot

"J'enregistre le capital obtenu par la mise en oeuvre de l'usage"
(myPlot remainRess_r &gt;= PickUse ressTapping)
ifTrue: [self capital: self capital + (PickUse incomesRate*PickUse ressTapping).
"Je consomme le nombre d'unités en NTFP de la parcelle qui correspond à l'usage (voir attribut de la classe PickUse) et s'il y en a moins,, je prends juste les unités en NTFP qui restent"
self seasonTappedRess_r: PickUse ressTapping. myPlot remainRess_r: (myPlot remainRess_r - PickUse ressTapping)]
ifFalse: [self capital: self capital + (PickUse incomesRate*myPlot remainRess_r). self seasonTappedRess_r: myPlot remainRess_r. myPlot remainRess_r: 0].</body>

<body package="WitUses">launchingPicking
	
	| myUse |

myUse:= 'Picking'.

self allocatingMyOwnSpusFor: myUse.</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.RessProvider class</class-id> <category>+ parameter unit</category>

<body package="WitUses">initRess_unit 
	"Return the unit of attribute #initRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressQuantity_type 
	"Return the type of attribute #ressQuantity.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">potentialRess_type 
	"Return the type of attribute #potentialRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ressType_type 
	"Return the type of attribute #ressType.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">remainRess_unit 
	"Return the unit of attribute #remainRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">remainRess_type 
	"Return the type of attribute #remainRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">potentialRess_unit 
	"Return the unit of attribute #potentialRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">initRess_type 
	"Return the type of attribute #initRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">providerType_type 
	"Return the type of attribute #providerType.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">ressQuantity_unit 
	"Return the unit of attribute #ressQuantity.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RessProvider class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RessProvider class</class-id> <category>- default value</category>

<body package="WitUses">potentialRess_default 
	"Return the default value of attribute #potentialRess : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ressType_default 
	"Return the default value of attribute #ressType : 'water'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'water'</body>

<body package="WitUses">remainRess_default 
	"Return the default value of attribute #remainRess : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">initRess_default 
	"Return the default value of attribute #initRess : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">providerType_default 
	"Return the default value of attribute #providerType : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ressQuantity_default 
	"Return the default value of attribute #ressQuantity : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RessProvider class</class-id> <category>pov symbols</category>

<body package="WitUses">VectorialPOV_povRemainRess
"Returns an array containing the settings of a Situated entity vectorial pov symbol in the following order:
(color red) (color green) (color blue) (lineColor red) (lineColor green) (lineColor blue) lineWidth sizeratio position (array of outline vertices) isDistortable." 
^#(0.0 0.0 1.0 0.0 0.0 0.0 1 1 #random #(4.45856e-7@-10.2 8.83346@-5.1 8.83346@5.1 -1.21634e-7@10.2 -8.83346@5.1 -8.83346@-5.1 3.44522e-6@-10.2) true)</body>

<body package="WitUses">VectorialPOV_RessProvider
"Returns an array containing the settings of a Situated entity vectorial pov symbol in the following order:
(color red) (color green) (color blue) (lineColor red) (lineColor green) (lineColor blue) lineWidth sizeratio position (array of outline vertices) isDistortable." 
^#(0.0 0.0 1.0 0.0 0.0 0.0 1 1 #random #(4.45856e-7@-10.2 8.83346@-5.1 8.83346@5.1 -1.21634e-7@10.2 -8.83346@5.1 -8.83346@-5.1 3.44522e-6@-10.2) true)</body>

<body package="WitUses">defaultColor 
"Returns the default color (Automatically defined by Cormas)." 
^ColorValue blue</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.RessProvider</class-id> <category>accessing</category>

<body package="WitUses">initRess: aUndefinedObject 
	"Setter accessor of attribute initRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	initRess := aUndefinedObject</body>

<body package="WitUses">remainRess
	"Getter accessor without initial value "
	^remainRess</body>

<body package="WitUses">ressType
	"Getter accessor with initial value = 'water'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressType ifNil:[ressType := 'water']</body>

<body package="WitUses">potentialRess: aSmallInteger 
	"Setter accessor of attribute potentialRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	potentialRess = aSmallInteger ifTrue:[^nil].   "for optimization"
	potentialRess := aSmallInteger. 
	self changed</body>

<body package="WitUses">providerType: aUndefinedObject 
	"Setter accessor of attribute providerType	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	providerType := aUndefinedObject</body>

<body package="WitUses">potentialRess
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^potentialRess ifNil:[potentialRess := 0]</body>

<body package="WitUses">remainRess: aUndefinedObject 
	"Setter accessor of attribute remainRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	remainRess = aUndefinedObject ifTrue:[^nil].   "for optimization"
	remainRess := aUndefinedObject. 
	self changed</body>

<body package="WitUses">ressType: aByteString 
	"Setter accessor of attribute ressType	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressType := aByteString</body>

<body package="WitUses">providerType
	"Getter accessor without initial value "
	^providerType</body>

<body package="WitUses">initRess
	"Getter accessor without initial value "
	^initRess</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RessProvider</class-id> <category>* pov</category>

<body package="WitUses">povRemainRess
"	Return a gradient color according to the remainRess value 
	(This method have been generated by Cormas)" 
	^self povAttribute: #remainRess  min: 0 color: ColorValue white max: 1.5 color: ColorValue blue</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.RessProvider</class-id> <category>control</category>

<body package="WitUses">ressVariability: weatherUncertainty and:geographicVariability
	
"impact de la variabilité sur le stock de ressources du ressProvider (la variable geographixVariability est différentes selon que la méthode est appelée dans EnVariability/climateUncertainty après être dans rulesImplementer southeast ou..) "

(self providerType = '1') ifTrue:[
((self patch x &gt; 6) and: [self patch y &gt; 6]) ifTrue: [self initRess: (self potentialRess*weatherUncertainty) + ((self potentialRess*weatherUncertainty)*geographicVariability)].
((self patch x &lt;= 6) and: [self patch y &gt; 6]) ifTrue: [self initRess: (self potentialRess*weatherUncertainty) + ((self potentialRess*weatherUncertainty)*geographicVariability)].
((self patch x &gt; 6) and: [self patch y &lt;= 6]) ifTrue: [self initRess: (self potentialRess*weatherUncertainty) + ((self potentialRess*weatherUncertainty)*geographicVariability) ].
((self patch x &lt;= 6) and: [self patch y &lt;= 6]) ifTrue: [self initRess: (self potentialRess*weatherUncertainty) + ((self potentialRess*weatherUncertainty)*geographicVariability)]
]</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.Plot class</class-id> <category>+ parameter unit</category>

<body package="WitUses">ress_Rs_Quantity_unit 
	"Return the unit of attribute #ress_Rs_Quantity.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">initRess_s_unit 
	"Return the unit of attribute #initRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">potentialRessSr_type 
	"Return the type of attribute #potentialRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">remainRess_s_type 
	"Return the type of attribute #remainRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">initRessRs_type 
	"Return the type of attribute #initRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">memoOverUseSr_type 
	"Return the type of attribute #memoOverUseSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overChargeSr_unit 
	"Return the unit of attribute #overChargeSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">protected_unit 
	"Return the unit of attribute #protected.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">initiRess_s_type 
	"Return the type of attribute #initiRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">overUse_r_unit 
	"Return the unit of attribute #overUse_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">remainRessRs_type 
	"Return the type of attribute #remainRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">potentialRess_r_unit 
	"Return the unit of attribute #potentialRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">potentialRessRs_type 
	"Return the type of attribute #potentialRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">memoOverUse_r_unit 
	"Return the unit of attribute #memoOverUse_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">initRess_s_type 
	"Return the type of attribute #initRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">initRessRS_type 
	"Return the type of attribute #initRessRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">closureDuration_type 
	"Return the type of attribute #closureDuration.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ress_Sr_type 
	"Return the type of attribute #ress_Sr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">holder_type 
	"Return the type of attribute #holder.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">remainRessSr_unit 
	"Return the unit of attribute #remainRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">memoOverUseRs_unit 
	"Return the unit of attribute #memoOverUseRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overUseRs_unit 
	"Return the unit of attribute #overUseRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">protected_type 
	"Return the type of attribute #protected.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overChargeSr_type 
	"Return the type of attribute #overChargeSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overUseSr_type 
	"Return the type of attribute #overUseSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">potentialRessSr_unit 
	"Return the unit of attribute #potentialRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">memoOverUseRs_type 
	"Return the type of attribute #memoOverUseRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">initiRess_r_unit 
	"Return the unit of attribute #initiRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">initRess_r_type 
	"Return the type of attribute #initRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overUseRs_type 
	"Return the type of attribute #overUseRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ress_RS_unit 
	"Return the unit of attribute #ress_RS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">remainRess_s_unit 
	"Return the unit of attribute #remainRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">closureDuration_unit 
	"Return the unit of attribute #closureDuration.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">potentialRess_s_type 
	"Return the type of attribute #potentialRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">overChargeRs_type 
	"Return the type of attribute #overChargeRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">initRessRS_unit 
	"Return the unit of attribute #initRessRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">potentialRess_s_unit 
	"Return the unit of attribute #potentialRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">memoOverUseSr_unit 
	"Return the unit of attribute #memoOverUseSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">landscape_type 
	"Return the type of attribute #landscape.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">ress_s_unit 
	"Return the unit of attribute #ress_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">potentialRessRS_unit 
	"Return the unit of attribute #potentialRessRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_s_type 
	"Return the type of attribute #ress_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">remainRess_r_unit 
	"Return the unit of attribute #remainRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_RS_initStock_type 
	"Return the type of attribute #ress_RS_initStock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">potentialRessRs_unit 
	"Return the unit of attribute #potentialRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_Sr_unit 
	"Return the unit of attribute #ress_Sr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_Rs_InitStock_unit 
	"Return the unit of attribute #ress_Rs_InitStock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">zoning_type 
	"Return the type of attribute #zoning.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">overChargeRs_unit 
	"Return the unit of attribute #overChargeRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">remainRessRS_type 
	"Return the type of attribute #remainRessRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">remainRess_r_type 
	"Return the type of attribute #remainRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">remainRessRs_unit 
	"Return the unit of attribute #remainRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">initiRess_s_unit 
	"Return the unit of attribute #initiRess_s.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_RS_type 
	"Return the type of attribute #ress_RS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">initRessRs_unit 
	"Return the unit of attribute #initRessRs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">initiRess_r_type 
	"Return the type of attribute #initiRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">initRessSr_unit 
	"Return the unit of attribute #initRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">potentialRessRS_type 
	"Return the type of attribute #potentialRessRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">ress_Rs_Quantity_type 
	"Return the type of attribute #ress_Rs_Quantity.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">ress_Rs_unit 
	"Return the unit of attribute #ress_Rs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">remainRessRS_unit 
	"Return the unit of attribute #remainRessRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_r_unit 
	"Return the unit of attribute #ress_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overUseSr_unit 
	"Return the unit of attribute #overUseSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_Rs_InitStock_type 
	"Return the type of attribute #ress_Rs_InitStock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">remainRessSr_type 
	"Return the type of attribute #remainRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ress_RS_initStock_unit 
	"Return the unit of attribute #ress_RS_initStock.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overUse_r_type 
	"Return the type of attribute #overUse_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ress_Rs_type 
	"Return the type of attribute #ress_Rs.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">initRessSr_type 
	"Return the type of attribute #initRessSr.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">initRess_r_unit 
	"Return the unit of attribute #initRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ress_r_type 
	"Return the type of attribute #ress_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">potentialRess_r_type 
	"Return the type of attribute #potentialRess_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">memoOverUse_r_type 
	"Return the type of attribute #memoOverUse_r.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">landscape_unit 
	"Return the unit of attribute #landscape.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Plot class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Plot class</class-id> <category>- default value</category>

<body package="WitUses">closureDuration_default 
	"Return the default value of attribute #closureDuration : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">memoOverUseSr_default 
	"Return the default value of attribute #memoOverUseSr : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">overUseSr_default 
	"Return the default value of attribute #overUseSr : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ress_RS_Quantity_default 
	"Return the default value of attribute #ress_RS_Quantity : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">zoning_default 
	"Return the default value of attribute #zoning : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">holder_default 
	"Return the default value of attribute #holder : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">overUseRs_default 
	"Return the default value of attribute #overUseRs : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">memoOverUse_r_default 
	"Return the default value of attribute #memoOverUse_r : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">remainRess_s_default 
	"Return the default value of attribute #remainRess_s : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ress_RS_default 
	"Return the default value of attribute #ress_RS : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">initiRess_s_default 
	"Return the default value of attribute #initiRess_s : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">overChargeSr_default 
	"Return the default value of attribute #overChargeSr : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">potentialRess_r_default 
	"Return the default value of attribute #potentialRess_r : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_Rs_InitStock_default 
	"Return the default value of attribute #ress_Rs_InitStock : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_Rs_initStock_default 
	"Return the default value of attribute #ress_Rs_initStock : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">remainRessRs_default 
	"Return the default value of attribute #remainRessRs : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ress_r_initStock_default 
	"Return the default value of attribute #ress_r_initStock : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_Sr_Quantity_default 
	"Return the default value of attribute #ress_Sr_Quantity : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_s_default 
	"Return the default value of attribute #ress_s : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_s_Quantity_default 
	"Return the default value of attribute #ress_s_Quantity : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">initRessRS_default 
	"Return the default value of attribute #initRessRS : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">initRess_s_default 
	"Return the default value of attribute #initRess_s : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">potentialRessRs_default 
	"Return the default value of attribute #potentialRessRs : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">initRessSr_default 
	"Return the default value of attribute #initRessSr : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ress_RS_initStock_default 
	"Return the default value of attribute #ress_RS_initStock : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_Rs_Quantity_default 
	"Return the default value of attribute #ress_Rs_Quantity : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">memoOverUseRs_default 
	"Return the default value of attribute #memoOverUseRs : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ress_Sr_default 
	"Return the default value of attribute #ress_Sr : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">remainRess_r_default 
	"Return the default value of attribute #remainRess_r : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">protected_default 
	"Return the default value of attribute #protected : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ress_s_initStock_default 
	"Return the default value of attribute #ress_s_initStock : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">landscape_default 
	"Return the default value of attribute #landscape : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">potentialRessRS_default 
	"Return the default value of attribute #potentialRessRS : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_Sr_initStock_default 
	"Return the default value of attribute #ress_Sr_initStock : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_Rs_default 
	"Return the default value of attribute #ress_Rs : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_r_default 
	"Return the default value of attribute #ress_r : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">overChargeRs_default 
	"Return the default value of attribute #overChargeRs : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">overUse_r_default 
	"Return the default value of attribute #overUse_r : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">initRessRs_default 
	"Return the default value of attribute #initRessRs : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">potentialRessSr_default 
	"Return the default value of attribute #potentialRessSr : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">ress_r_Quantity_default 
	"Return the default value of attribute #ress_r_Quantity : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">remainRessSr_default 
	"Return the default value of attribute #remainRessSr : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">potentialRess_s_default 
	"Return the default value of attribute #potentialRess_s : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">initRess_r_default 
	"Return the default value of attribute #initRess_r : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">remainRessRS_default 
	"Return the default value of attribute #remainRessRS : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">initiRess_r_default 
	"Return the default value of attribute #initiRess_r : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Plot class</class-id> <category>pov symbols</category>

<body package="WitUses">SpatialEntityPOV_MobilePicking
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 0.540105 with: 0.810035 with: 0.900012</body>

<body package="WitUses">SpatialEntityPOV_Settled
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 0.0 with: 1.0 with: 0.720059</body>

<body package="WitUses">SpatialEntityPOV_Picking
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 0.900012 with: 0.835063 with: 0.816506</body>

<body package="WitUses">defaultColor
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 1.0 with: 1.0 with: 1.0</body>

<body package="WitUses">SpatialEntityPOV_II
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 1.0 with: 0.729947 with: 0.0</body>

<body package="WitUses">SpatialEntityPOV_hotPlot
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 1.0 with: 0.0 with: 0.0</body>

<body package="WitUses">SpatialEntityPOV_I
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 0.450006 with: 0.0 with: 0.0</body>

<body package="WitUses">SpatialEntityPOV_SettledPicking
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 0.700037 with: 0.900012 with: 0.739958</body>

<body package="WitUses">SpatialEntityPOV_IV
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 0.0 with: 0.670004 with: 0.0</body>

<body package="WitUses">SpatialEntityPOV_III
"Returns an array containing the settings of a Spatial entity pov symbol in the following order:
(color red) (color green) (color blue)
(Automatically defined by Cormas)."
^Array with: 1.0 with: 1.0 with: 0.0</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.Plot</class-id> <category>accessing</category>

<body package="WitUses">remainRessSr: anObject
	"Setter accessor of attribute remainRessSr. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	remainRessSr = anObject ifTrue:[^nil].   "for optimization"
	remainRessSr := anObject.
	self changed</body>

<body package="WitUses">remainRess_r
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^remainRess_r ifNil:[remainRess_r := 0]</body>

<body package="WitUses">initRess_s: aSmallInteger 
	"Setter accessor of attribute initRess_s	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	initRess_s = aSmallInteger ifTrue:[^nil].   "for optimization"
	initRess_s := aSmallInteger. 
	self changed</body>

<body package="WitUses">memoOverUse_r
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^memoOverUse_r ifNil:[memoOverUse_r := 0]</body>

<body package="WitUses">holder
	"Getter accessor without initial value "
	^holder</body>

<body package="WitUses">potentialRessRS
	"Getter accessor without initial value "
	^potentialRessRS</body>

<body package="WitUses">remainRess_r: anObject
	"Setter accessor of attribute remainRess_r. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	remainRess_r = anObject ifTrue:[^nil].   "for optimization"
	remainRess_r := anObject.
	self changed</body>

<body package="WitUses">remainRessSr
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^remainRessSr ifNil:[remainRessSr := 0]</body>

<body package="WitUses">remainRess_s: anObject
	"Setter accessor of attribute remainRess_s. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	remainRess_s = anObject ifTrue:[^nil].   "for optimization"
	remainRess_s := anObject.
	self changed</body>

<body package="WitUses">overUseRs: aSmallInteger 
	"Setter accessor of attribute overUseRs	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overUseRs = aSmallInteger ifTrue:[^nil].   "for optimization"
	overUseRs := aSmallInteger. 
	self changed</body>

<body package="WitUses">overUse_r
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overUse_r ifNil:[overUse_r := 0]</body>

<body package="WitUses">initRess_s
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^initRess_s ifNil:[initRess_s := 0]</body>

<body package="WitUses">initRess_r
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^initRess_r ifNil:[initRess_r := 0]</body>

<body package="WitUses">overUse_r: aSmallInteger 
	"Setter accessor of attribute overUse_r	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overUse_r = aSmallInteger ifTrue:[^nil].   "for optimization"
	overUse_r := aSmallInteger. 
	self changed</body>

<body package="WitUses">remainRessRS
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^remainRessRS ifNil:[remainRessRS := 0]</body>

<body package="WitUses">landscape
	"Getter accessor without initial value "
	^landscape</body>

<body package="WitUses">overUseSr
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overUseSr ifNil:[overUseSr := 0]</body>

<body package="WitUses">landscape: aUndefinedObject 
	"Setter accessor of attribute landscape	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landscape := aUndefinedObject</body>

<body package="WitUses">remainRessRs
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^remainRessRs ifNil:[remainRessRs := 0]</body>

<body package="WitUses">memoOverUseSr
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^memoOverUseSr ifNil:[memoOverUseSr := 0]</body>

<body package="WitUses">memoOverUseSr: aSmallInteger 
	"Setter accessor of attribute memoOverUseSr	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	memoOverUseSr = aSmallInteger ifTrue:[^nil].   "for optimization"
	memoOverUseSr := aSmallInteger. 
	self changed</body>

<body package="WitUses">potentialRess_s: aUndefinedObject 
	"Setter accessor of attribute potentialRess_s	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	potentialRess_s = aUndefinedObject ifTrue:[^nil].   "for optimization"
	potentialRess_s := aUndefinedObject. 
	self changed</body>

<body package="WitUses">potentialRessRs: aUndefinedObject 
	"Setter accessor of attribute potentialRessRs	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	potentialRessRs = aUndefinedObject ifTrue:[^nil].   "for optimization"
	potentialRessRs := aUndefinedObject. 
	self changed</body>

<body package="WitUses">potentialRessSr
	"Getter accessor without initial value "
	^potentialRessSr</body>

<body package="WitUses">remainRessRs: anObject
	"Setter accessor of attribute remainRessRs. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	remainRessRs = anObject ifTrue:[^nil].   "for optimization"
	remainRessRs := anObject.
	self changed</body>

<body package="WitUses">initRessRs
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^initRessRs ifNil:[initRessRs := 0]</body>

<body package="WitUses">memoOverUseRs: aSmallInteger 
	"Setter accessor of attribute memoOverUseRs	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	memoOverUseRs = aSmallInteger ifTrue:[^nil].   "for optimization"
	memoOverUseRs := aSmallInteger. 
	self changed</body>

<body package="WitUses">initRess_r: aSmallInteger 
	"Setter accessor of attribute initRess_r	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	initRess_r := aSmallInteger</body>

<body package="WitUses">potentialRess_r: aUndefinedObject 
	"Setter accessor of attribute potentialRess_r	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	potentialRess_r = aUndefinedObject ifTrue:[^nil].   "for optimization"
	potentialRess_r := aUndefinedObject. 
	self changed</body>

<body package="WitUses">initRessRS
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^initRessRS ifNil:[initRessRS := 0]</body>

<body package="WitUses">initRessSr: aSmallInteger 
	"Setter accessor of attribute initRessSr	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	initRessSr = aSmallInteger ifTrue:[^nil].   "for optimization"
	initRessSr := aSmallInteger. 
	self changed</body>

<body package="WitUses">potentialRess_r
	"Getter accessor without initial value "
	^potentialRess_r</body>

<body package="WitUses">zoning
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^zoning ifNil:[zoning := '']</body>

<body package="WitUses">closureDuration: aSmallInteger 
	"Setter accessor of attribute closureDuration	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	closureDuration = aSmallInteger ifTrue:[^nil].   "for optimization"
	closureDuration := aSmallInteger. 
	self changed</body>

<body package="WitUses">potentialRessRs
	"Getter accessor without initial value "
	^potentialRessRs</body>

<body package="WitUses">potentialRessRS: aUndefinedObject 
	"Setter accessor of attribute potentialRessRS	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	potentialRessRS = aUndefinedObject ifTrue:[^nil].   "for optimization"
	potentialRessRS := aUndefinedObject. 
	self changed</body>

<body package="WitUses">potentialRessSr: aUndefinedObject 
	"Setter accessor of attribute potentialRessSr	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	potentialRessSr = aUndefinedObject ifTrue:[^nil].   "for optimization"
	potentialRessSr := aUndefinedObject. 
	self changed</body>

<body package="WitUses">remainRess_s
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^remainRess_s ifNil:[remainRess_s := 0]</body>

<body package="WitUses">memoOverUse_r: aSmallInteger 
	"Setter accessor of attribute memoOverUse_r	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	memoOverUse_r = aSmallInteger ifTrue:[^nil].   "for optimization"
	memoOverUse_r := aSmallInteger. 
	self changed</body>

<body package="WitUses">initRessRS: aSmallInteger 
	"Setter accessor of attribute initRessRS	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	initRessRS = aSmallInteger ifTrue:[^nil].   "for optimization"
	initRessRS := aSmallInteger. 
	self changed</body>

<body package="WitUses">zoning: aByteString 
	"Setter accessor of attribute zoning	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	zoning = aByteString ifTrue:[^nil].   "for optimization"
	zoning := aByteString. 
	self changed</body>

<body package="WitUses">overUseSr: aSmallInteger 
	"Setter accessor of attribute overUseSr	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overUseSr = aSmallInteger ifTrue:[^nil].   "for optimization"
	overUseSr := aSmallInteger. 
	self changed</body>

<body package="WitUses">closureDuration
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^closureDuration ifNil:[closureDuration := 0]</body>

<body package="WitUses">initRessSr
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^initRessSr ifNil:[initRessSr := 0]</body>

<body package="WitUses">holder: aUndefinedObject 
	"Setter accessor of attribute holder	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	holder := aUndefinedObject</body>

<body package="WitUses">memoOverUseRs
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^memoOverUseRs ifNil:[memoOverUseRs := 0]</body>

<body package="WitUses">potentialRess_s
	"Getter accessor without initial value "
	^potentialRess_s</body>

<body package="WitUses">overUseRs
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overUseRs ifNil:[overUseRs := 0]</body>

<body package="WitUses">initRessRs: aSmallInteger 
	"Setter accessor of attribute initRessRs	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	initRessRs = aSmallInteger ifTrue:[^nil].   "for optimization"
	initRessRs := aSmallInteger. 
	self changed</body>

<body package="WitUses">remainRessRS: anObject
	"Setter accessor of attribute remainRessRS. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	remainRessRS = anObject ifTrue:[^nil].   "for optimization"
	remainRessRS := anObject.
	self changed</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Plot</class-id> <category>* pov</category>

<body package="WitUses">povRemainRess_r
"	Return a gradient color according to the remainRess_r value 
	(This method have been generated by Cormas)" 
	^self povAttribute: #remainRess_r  min: 0 color: ColorValue white max: 0.3 color: (ColorValue red: 0.633012 green: 0.0 blue: 0.633012)</body>

<body package="WitUses">povZoning
"return the value of the attribute
if the value is a string, converts it to a symbol
if the value is a number, converts it to a symbol, precede by the word zoning"
	self zoning isString ifTrue:[^self zoning asSymbol].
	self zoning isLiteral ifTrue:[^ ('zoning' , self zoning printString) asSymbol].
	^self zoning</body>

<body package="WitUses">povLandscape
"return the value of the attribute
if the value is a string, converts it to a symbol
if the value is a number, converts it to a symbol, precede by the word landscape"
	self landscape isString ifTrue:[^self landscape asSymbol].
	self landscape isLiteral ifTrue:[^ ('landscape' , self landscape printString) asSymbol].
	^self landscape</body>

<body package="WitUses">povRemainRessRS
"	Return a gradient color according to the remainRessRS value 
	(This method have been generated by Cormas)" 
	^self povAttribute: #remainRessRS  min: 0 color: ColorValue white max: 1.5 color: (ColorValue red: 0.0 green: 0.500061 blue: 1.0)</body>

<body package="WitUses">povRemainRessRs
"	Return a gradient color according to the remainRessRs value 
	(This method have been generated by Cormas)" 
	^self povAttribute: #remainRessRs  min: 0 color: ColorValue white max: 0.65  color: (ColorValue red: 0.0 green: 0.482969 blue: 0.0)</body>

<body package="WitUses">povRemainRessSr
"	Return a gradient color according to the remainRessSr value 
	(This method have been generated by Cormas)" 
	^self povAttribute: #remainRessSr  min: 0 color: ColorValue white max: 0.5 color: ColorValue orange</body>

<body package="WitUses">povHolder


    self holder ifNil: [^ ColorValue white].  "si pas de holder, renvoie blanc"
(self holder class = RulesImplementer) ifTrue: [^ ColorValue black]. "si RulesImplementer, renvoie black"
^self class colorOfNumber: self holder id "si user return id color"</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Plot</class-id> <category>control</category>

<body package="WitUses">overUseMemory

(self remainRessRs = 0) ifTrue: [self overUseRs: self overUseRs + 1].
(self remainRessSr = 0) ifTrue: [self overUseSr: self overUseSr + 1].
(self remainRess_r = 0) ifTrue: [self overUse_r: self overUse_r + 1].</body>

<body package="WitUses">plotRessVariability: weatherUncertainty and:geographicVariability

	| plotVariability|

"la ressource Sr (sol) n'est pas touchée par le climat, ainsi que les Ressproviders de type 2"
"plotVariability:= Cormas selectRandomlyFrom: (List withAll: #(0))."
plotVariability:= Cormas selectRandomlyFrom: (List withAll: #(-0.65 -0.5 -0.25 -0.1 0 0.1 0.25 0.5 0.65)).

self initRessRS: (self potentialRessRS*weatherUncertainty) + ((self potentialRessRS*weatherUncertainty)*geographicVariability) + (((self potentialRessRS*weatherUncertainty)*geographicVariability)*plotVariability).
self initRessRs: (self potentialRessRs*weatherUncertainty) + ((self potentialRessRs*weatherUncertainty)*geographicVariability) + (((self potentialRessRs*weatherUncertainty)*geographicVariability)*plotVariability).
self initRess_r: (self potentialRess_r*weatherUncertainty) + ((self potentialRess_r*weatherUncertainty)*geographicVariability) + (((self potentialRess_r*weatherUncertainty)*geographicVariability)*plotVariability).</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SouthWest class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SouthWest class</class-id> <category>accessing</category>

<body package="WitUses">myTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myTerritory ifNil:[myTerritory := OrderedCollection new]</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SouthWest</class-id> <category>control</category>

<body package="WitUses">initMyTerritory
	"comment stating purpose of message"</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SouthWest</class-id> <category>accessing</category>

<body package="WitUses">envRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^envRule ifNil:[envRule := '']</body>

<body package="WitUses">landTenureRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landTenureRule ifNil:[landTenureRule := '']</body>

<body package="WitUses">backUpOfAllSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpOfAllSeasonPlotUses ifNil:[backUpOfAllSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">landUsesRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landUsesRule ifNil:[landUsesRule := '']</body>

<body package="WitUses">sahelianRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sahelianRule ifNil:[sahelianRule := '']</body>

<body package="WitUses">backUpAppliedRules
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpAppliedRules ifNil:[backUpAppliedRules := OrderedCollection new]</body>

<body package="WitUses">mySeasonRessProviderUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonRessProviderUses ifNil:[mySeasonRessProviderUses := OrderedCollection new]</body>

<body package="WitUses">socialRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^socialRule ifNil:[socialRule := '']</body>

<body package="WitUses">backUpMyTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpMyTerritory ifNil:[backUpMyTerritory := OrderedCollection new]</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.NorthWest class</class-id> <category>+ parameter unit</category>

<body package="WitUses">landUsesRule_type 
	"Return the type of attribute #landUsesRule.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthWest class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthWest class</class-id> <category>accessing</category>

<body package="WitUses">myTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myTerritory ifNil:[myTerritory := OrderedCollection new]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthWest class</class-id> <category>- default value</category>

<body package="WitUses">landUsesRule_default 
	"Return the default value of attribute #landUsesRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.NorthWest</class-id> <category>control</category>

<body package="WitUses">initMyTerritory
	"comment stating purpose of message"

WitUses thePlots do: [:p | ((p x &lt;= 6) and: [p y &lt;= 6]) ifTrue: [self myTerritory add: p]].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.NorthWest</class-id> <category>accessing</category>

<body package="WitUses">landTenureRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landTenureRule ifNil:[landTenureRule := '']</body>

<body package="WitUses">backUpOfAllSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpOfAllSeasonPlotUses ifNil:[backUpOfAllSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">landUsesRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landUsesRule ifNil:[landUsesRule := '']</body>

<body package="WitUses">sahelianRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sahelianRule ifNil:[sahelianRule := '']</body>

<body package="WitUses">backUpAppliedRules
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpAppliedRules ifNil:[backUpAppliedRules := OrderedCollection new]</body>

<body package="WitUses">mySeasonRessProviderUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonRessProviderUses ifNil:[mySeasonRessProviderUses := OrderedCollection new]</body>

<body package="WitUses">socialRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^socialRule ifNil:[socialRule := '']</body>

<body package="WitUses">backUpMyTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpMyTerritory ifNil:[backUpMyTerritory := OrderedCollection new]</body>

<body package="WitUses">landUsesRule: aByteString 
	"Setter accessor of attribute landUsesRule	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landUsesRule = aByteString ifTrue:[^nil].   "for optimization"
	landUsesRule := aByteString. 
	self changed</body>

<body package="WitUses">envRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^envRule ifNil:[envRule := '']</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.PickUse class</class-id> <category>+ parameter unit</category>

<body package="WitUses">incomes_unit 
	"Return the unit of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">requiredCapital_unit 
	"Return the unit of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">incomesRate_type 
	"Return the type of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">needRS_unit 
	"Return the unit of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">resilienceRatioRess_type 
	"Return the type of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">requiredInvest_unit 
	"Return the unit of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overRessUseRatio_unit 
	"Return the unit of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overRessUseRatio_type 
	"Return the type of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">incomesRate_unit 
	"Return the unit of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overYearlyThresholdRess_unit 
	"Return the unit of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressTapping_type 
	"Return the type of attribute #ressTapping.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">overYearlyThresholdRess_type 
	"Return the type of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">needRS_type 
	"Return the type of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">requiredCapital_type 
	"Return the type of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">use_type 
	"Return the type of attribute #use.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">requiredInvest_type 
	"Return the type of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">incomes_type 
	"Return the type of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">ressTapping_unit 
	"Return the unit of attribute #ressTapping.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">resource_type 
	"Return the type of attribute #resource.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">resilienceRatioRess_unit 
	"Return the unit of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.PickUse class</class-id> <category>accessing</category>

<body package="WitUses">myRadius
	"Getter accessor without initial value "
	^myRadius</body>

<body package="WitUses">overRessUseRatio: aFloat 
	"Setter accessor of attribute overRessUseRatio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overRessUseRatio := aFloat</body>

<body package="WitUses">requiredCapital
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^requiredCapital ifNil:[requiredCapital := 0]</body>

<body package="WitUses">ressTapping
	"Getter accessor with initial value = 0.3. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressTapping ifNil:[ressTapping := 0.3]</body>

<body package="WitUses">overRessUseRatio
	"Getter accessor with initial value = 0.6. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overRessUseRatio ifNil:[overRessUseRatio := 0.6]</body>

<body package="WitUses">use
	"Getter accessor with initial value = 'Pick'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := 'Pick']</body>

<body package="WitUses">resilienceRatioRess: aSmallInteger 
	"Setter accessor of attribute resilienceRatioRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	resilienceRatioRess := aSmallInteger</body>

<body package="WitUses">resilienceRatioRess
	"Getter accessor with initial value = 5. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^resilienceRatioRess ifNil:[resilienceRatioRess := 5]</body>

<body package="WitUses">incomesRate
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^incomesRate ifNil:[incomesRate := 1]</body>

<body package="WitUses">resource
	"Getter accessor with initial value = 'r'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^resource ifNil:[resource := 'r']</body>

<body package="WitUses">ressTapping: aFloat 
	"Setter accessor of attribute ressTapping	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressTapping := aFloat</body>

<body package="WitUses">requiredInvest
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^requiredInvest ifNil:[requiredInvest := 0]</body>

<body package="WitUses">overYearlyThresholdRess
	"Getter accessor with initial value = 3. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overYearlyThresholdRess ifNil:[overYearlyThresholdRess := 3]</body>

<body package="WitUses">resource: aByteString 
	"Setter accessor of attribute resource	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	resource := aByteString</body>

<body package="WitUses">needRS
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^needRS ifNil:[needRS := 0]</body>

<body package="WitUses">requiredInvest: aSmallInteger 
	"Setter accessor of attribute requiredInvest	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredInvest := aSmallInteger</body>

<body package="WitUses">requiredCapital: aSmallInteger 
	"Setter accessor of attribute requiredCapital	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredCapital := aSmallInteger</body>

<body package="WitUses">incomesRate: aSmallInteger 
	"Setter accessor of attribute incomesRate	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	incomesRate := aSmallInteger</body>

<body package="WitUses">overYearlyThresholdRess: aSmallInteger 
	"Setter accessor of attribute overYearlyThresholdRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overYearlyThresholdRess := aSmallInteger</body>

<body package="WitUses">use: aByteString 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use = aByteString ifTrue:[^nil].   "for optimization"
	use := aByteString. 
	self changed</body>

<body package="WitUses">needRS: aSmallInteger 
	"Setter accessor of attribute needRS	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	needRS = aSmallInteger ifTrue:[^nil].   "for optimization"
	needRS := aSmallInteger. 
	self changed</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.PickUse class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.PickUse class</class-id> <category>- default value</category>

<body package="WitUses">overRessUseRatio_default 
	"Return the default value of attribute #overRessUseRatio : 0.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.6</body>

<body package="WitUses">ressTapping_default 
	"Return the default value of attribute #ressTapping : 0.3.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.3</body>

<body package="WitUses">requiredInvest_default 
	"Return the default value of attribute #requiredInvest : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">use_default 
	"Return the default value of attribute #use : 'Pick'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'Pick'</body>

<body package="WitUses">needRS_default 
	"Return the default value of attribute #needRS : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">overYearlyThresholdRess_default 
	"Return the default value of attribute #overYearlyThresholdRess : 3.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^3</body>

<body package="WitUses">incomes_default 
	"Return the default value of attribute #incomes : 0.25.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.25</body>

<body package="WitUses">resilienceRatioRess_default 
	"Return the default value of attribute #resilienceRatioRess : 5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^5</body>

<body package="WitUses">resource_default 
	"Return the default value of attribute #resource : 'r'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'r'</body>

<body package="WitUses">requiredCapital_default 
	"Return the default value of attribute #requiredCapital : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">incomesRate_default 
	"Return the default value of attribute #incomesRate : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.WitUses class</class-id> <category>description</category>

<body package="WitUses">lastUpdate
^ 'mardi 22 septembre 2020'</body>

<body package="WitUses">comments
^ ''</body>

<body package="WitUses">creationDate
^ 'jeudi 27 septembre 2018'</body>

<body package="WitUses">version
^ 'WitUses.st'</body>

<body package="WitUses">currentDeveloperName
^ 'Patrick D"aquino '</body>

<body package="WitUses">cormasCompatibleVersion
^ #(2020 3 31 6 38 27)</body>

<body package="WitUses">versionComments
^ ' 

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 09 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses test.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses test.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses test.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 30 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 31 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 31 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 31 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 31 10 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 11 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses XX 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 27 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 28 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 12 18.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 01 19 SPU ss classes.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPUb pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 01 19 SPU pasvariaclass.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 01 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 06b 02 19 qui tourne sans rulesimplementer.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 11 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 11 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 11 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12b 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 13 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 22 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 22 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 23 02 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 03 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 03 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 03 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 03 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 03 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 03 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 08 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 11 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 11 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 12 07 19.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 07 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 10 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 11 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 11 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20 no processinguses.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 14 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 15 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 16 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 17 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20b.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 18 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 19 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 20 02 20 agri.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 21 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 22 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 22 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 22 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 22 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20b multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20b multiUse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 24 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 livingStandard.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 calé.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 calé.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 calé.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 25 02 20 calé.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 26 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 29 02 20 overuse.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 01 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 02 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 04 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20 testPriority.pcl --------------------- Date: 2018.9.3

 *** Developer:   ***

 --------------------- WitUses 05 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 good.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 noPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 03 20 testPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 whileTrue.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 whileTrue.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 whileTrue.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 whileTrue.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 2times.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 2times.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 03 20 2times.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 1ç 03 20 2times.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 03 20 2times.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 03 20 2times.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 03 20 2times test.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 03 20 2times test.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 03 20 STABLE MOBILE.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 21 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 23 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF02 23 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF MOBILE 23 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF SETTLED 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF SETTLED 24 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF PICKING 25 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF MOBILE 26 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 27 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF MULTIUSES 27 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF MULTIUSES 27 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses REF MULTIUSES 27 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 29 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 29 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 29 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 29 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 29 03 20 REF CLIMAT.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 30 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRS.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 NoVariability.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 NoVariability.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 NoVariability.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 NoVariability.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRS.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRS.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRS.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRessProv.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRessProv.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRessProv.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityButNoForRessProv.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 31 03 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAllb.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAllb.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAllb.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAllb.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 02 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 04 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 04 20 VariabilityForAll.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation test.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation S0.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation NO UNCERTAINTY.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation NO UNCERTAINTY.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation NO UNCERTAINTY.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation NO UNCERTAINTY.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 04 20 TEST.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 04 20 TEST.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 04 20 TEST.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 04 20 Situation powerful settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation settled.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation mobile.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation mobile.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation mobile.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation mobile.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 04 20 Situation mobile.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 14 04 20 Situation mobile.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20 tous separes.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20 tous separes.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20 tous separes.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20 tous separes B.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20 tous separes B.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20 tous separes B.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 04 20 tous separes B.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous separes.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+1.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 04 20 tous ensemble OLD+2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 04 20 tous ensemble NEW.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 27 04 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 onePlotOneUser.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 onePlotOneUser.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 locatedTerritories.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 locatedTerritories.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 groupPriority.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 groupPriority.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 socialNetwork.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 socialNetwork.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 socialNetwork.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 28 04 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 05 20 rateTotalClosingNew.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 05 20 rateTotalClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 05 20 rateTotalClosing New2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 05 20 rateTotalClosing New2.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing OK.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing New3.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing New3.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing New3.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing New3.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing New3.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 rateTotalClosing New3.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 ratePlotRessClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 ratePlotRessClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 ratePlotRessClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 06 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 terrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 terrTotClosingb.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 terrRessClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 terrRessClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 seasonPlotTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 07 05 20 seasonPlotTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 05 20 seasonPlotTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 05 20 seasonPlotTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 05 20 seasonPlotTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 05 20 seasonPlotTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 05 20 seasonTerrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 05 20b seasonTerrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 08 05 20b seasonTerrTotClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 05 20 seasonTerrRessClosing.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 05 20 livingStandardPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 envPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 spatializingOnePlotOneUser.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 05 20 spatializingOnePlotOneUser.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 14 05 20 spatializingOnePlotOneUser.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 14 05 20 spatializingPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 14 05 20 onePlotOneUserdifficult.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 onePlotOneUserdifficult.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 onePlotOneUserdifficult.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 onePlotOneUserdifficult.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 onePlotOneUserdifficult.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 onePlotOneUserdifficult.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 spatializingPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 spatializingPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 spatializingPoliciesOK.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 spatializingPoliciesOK.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 spatializingPoliciesOK.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 livelihoodPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 livelihoodPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 livelihoodPolicies OK.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 crisisPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 05 20 crisisPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 05 20 weatherCrisisPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 05 20 weatherCrisisPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 05 20 weatherCrisisPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 05 20 weatherCrisisPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 05 20 weatherCrisisPolicy.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 21 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 21 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 05 20 crisisPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 06 20 zoningPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 06 20 zoningPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 06 20 zoningPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 06 20 zoningPolicies.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 06 20 zoningAreas.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 06 20 zoningAreas.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 03 06 20 zoningAreas.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 04 06 20 4 Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 04 06 20 4 Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 04 06 20 4 Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 04 06 20 4 Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 04 06 20 4 Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 05 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 09 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2018.9.3

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 10 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 11 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 12 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 13 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 15 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 16 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 17 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 18 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 19 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 20 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 21 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 21 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 23 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five ImplementersB.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five ImplementersB.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 24 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 25 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementersb.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementersb.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 26 06 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 01 07 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses 22 09 20 Five Implementers.pcl --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***

 --------------------- WitUses.st --------------------- Date: 2020.3.31

 *** Developer: Patrick D"aquino  ***'</body>

<body package="WitUses">emails
^ ' '</body>

<body package="WitUses">vwCompatibleVersion
^ '7.6'</body>

<body package="WitUses">authors
^ ' '</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.WitUses class</class-id> <category>+ parameter unit</category>

<body package="WitUses">numOfNorthWests_unit 
	"Return the unit of attribute #numOfNorthWests.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfGlobals_unit 
	"Return the unit of attribute #numOfGlobals.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfUsers_unit 
	"Return the unit of attribute #numOfUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfMobileUses_unit 
	"Return the unit of attribute #numOfMobileUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfUsers_type 
	"Return the type of attribute #numOfUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfSetlledUses_unit 
	"Return the unit of attribute #numOfSetlledUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfOveralls_type 
	"Return the type of attribute #numOfOveralls.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfIntensiveUses_unit 
	"Return the unit of attribute #numOfIntensiveUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfIntensiveUses_type 
	"Return the type of attribute #numOfIntensiveUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfMonitorings_type 
	"Return the type of attribute #numOfMonitorings.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfMonitorings_unit 
	"Return the unit of attribute #numOfMonitorings.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfNorthEasts_type 
	"Return the type of attribute #numOfNorthEasts.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfSouthWests_type 
	"Return the type of attribute #numOfSouthWests.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfSouthEasts_type 
	"Return the type of attribute #numOfSouthEasts.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfGlobals_type 
	"Return the type of attribute #numOfGlobals.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfNorthEasts_unit 
	"Return the unit of attribute #numOfNorthEasts.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfNorthWests_type 
	"Return the type of attribute #numOfNorthWests.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfPickUses_unit 
	"Return the unit of attribute #numOfPickUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfSouthWests_unit 
	"Return the unit of attribute #numOfSouthWests.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfOveralls_unit 
	"Return the unit of attribute #numOfOveralls.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfPolicyMakers_unit 
	"Return the unit of attribute #numOfPolicyMakers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfSimulationParameterss_unit 
	"Return the unit of attribute #numOfSimulationParameterss.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfSetlledUses_type 
	"Return the type of attribute #numOfSetlledUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfPolicyMakers_type 
	"Return the type of attribute #numOfPolicyMakers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfSouthEasts_unit 
	"Return the unit of attribute #numOfSouthEasts.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">numOfSimulationParameterss_type 
	"Return the type of attribute #numOfSimulationParameterss.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfMobileUses_type 
	"Return the type of attribute #numOfMobileUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">numOfPickUses_type 
	"Return the type of attribute #numOfPickUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.WitUses class</class-id> <category>- probes</category>

<body package="WitUses">colorOfProbe_sumInitRessRSOfPlots
  ^ ColorValue cyan</body>

<body package="WitUses">colorOfProbe_sumSeasonTappedRessRS_MobileOfUsers
  ^ (ColorValue red: 0.0 green: 0.624954 blue: 0.0)</body>

<body package="WitUses">colorOfProbe_sumRemainRessRSOfPlots
  ^ (ColorValue red: 0.250031 green: 0.624954 blue: 1.0)</body>

<body package="WitUses">colorOfProbe_sumInitRessSrOfPlots
  ^ (ColorValue red: 0.642046 green: 0.320352 blue: 0.0)</body>

<body package="WitUses">colorOfProbe_sumMyLivestockOfUsers
  ^ ColorValue veryDarkGray</body>

<body package="WitUses">colorOfProbe_sumSeasonTappedRessSrOfUsers
  ^ (ColorValue red: 0.558052 green: 0.506409 blue: 0.0417531)</body>

<body package="WitUses">colorOfProbe_varianceCapitalOfUsers
  ^ ColorValue orange</body>

<body package="WitUses">colorOfProbe_sumRemainRessOfRessProviders
  ^ ColorValue blue</body>

<body package="WitUses">colorOfProbe_sumYearlyTappedRessRS_MobileOfUsers
  ^ (ColorValue red: 0.0 green: 0.742034 blue: 0.0)</body>

<body package="WitUses">colorOfProbe_sumYearlyTappedRessRS_SettledOfUsers
  ^ (ColorValue red: 0.582957 green: 0.0 blue: 0.0)</body>

<body package="WitUses">colorOfProbe_sumYearlyTappedRess_rOfUsers
  ^ ColorValue darkRed</body>

<body package="WitUses">colorOfProbe_maxCapitalOfUsers
  ^ (ColorValue red: 0.624954 green: 0.250031 blue: 1.0)</body>

<body package="WitUses">colorOfProbe_sumYearlyTappedRessRsOfUsers
  ^ (ColorValue red: 0.0 green: 0.558052 blue: 0.0)</body>

<body package="WitUses">colorOfProbe_sumSeasonTappedRessRS_SettledOfUsers
  ^ (ColorValue red: 0.675009 green: 0.337077 blue: 0.0)</body>

<body package="WitUses">colorOfProbe_sumRemainRessSrOfPlots
  ^ (ColorValue red: 0.842022 green: 0.420828 blue: 0.0)</body>

<body package="WitUses">colorOfProbe_sumYearlyTappedRessSrOfUsers
  ^ ColorValue darkMagenta</body>

<body package="WitUses">colorOfProbe_sumSeasonTappedRessRsOfUsers
  ^ ColorValue darkCyan</body>

<body package="WitUses">colorOfProbe_sumInitRess_rOfPlots
  ^ (ColorValue red: 1.0 green: 0.125015 blue: 1.0)</body>

<body package="WitUses">colorOfProbe_averageYearlyTappedRessRS_SettledOfUsers
  ^ ColorValue red</body>

<body package="WitUses">colorOfProbe_sumCapitalOfUsers
  ^ (ColorValue red: 1.0 green: 0.624954 blue: 0.250031)</body>

<body package="WitUses">colorOfProbe_sumRemainRess_rOfPlots
  ^ (ColorValue red: 0.717006 green: 0.0 blue: 0.717006)</body>

<body package="WitUses">colorOfProbe_sumNbUsersPositifCapitalOfMonitorings
  ^ ColorValue veryLightGray</body>

<body package="WitUses">colorOfProbe_sumRemainRessRsOfPlots
  ^ (ColorValue red: 0.0 green: 0.624954 blue: 0.312416)</body>

<body package="WitUses">colorOfProbe_sumSeasonTappedRess_rOfUsers
  ^ (ColorValue red: 0.749969 green: 0.0 blue: 0.749969)</body>

<body package="WitUses">colorOfProbe_sumMyHungryLivestockOfUsers
  ^ ColorValue red</body>

<body package="WitUses">colorOfProbe_sumInitRessRsOfPlots
  ^ (ColorValue red: 0.0 green: 0.357954 blue: 0.0)</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.WitUses class</class-id> <category>- default value</category>

<body package="WitUses">numOfOveralls_default 
	"Return the default value of attribute #numOfOveralls : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfIntensiveUses_default 
	"Return the default value of attribute #numOfIntensiveUses : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfSouthWests_default 
	"Return the default value of attribute #numOfSouthWests : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfSouthEasts_default 
	"Return the default value of attribute #numOfSouthEasts : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfUsers_default 
	"Return the default value of attribute #numOfUsers : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfSetlledUses_default 
	"Return the default value of attribute #numOfSetlledUses : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfNorthWests_default 
	"Return the default value of attribute #numOfNorthWests : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfMobileUses_default 
	"Return the default value of attribute #numOfMobileUses : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfNorthEasts_default 
	"Return the default value of attribute #numOfNorthEasts : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfSimulationParameterss_default 
	"Return the default value of attribute #numOfSimulationParameterss : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfGlobals_default 
	"Return the default value of attribute #numOfGlobals : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfMonitorings_default 
	"Return the default value of attribute #numOfMonitorings : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfPickUses_default 
	"Return the default value of attribute #numOfPickUses : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">numOfPolicyMakers_default 
	"Return the default value of attribute #numOfPolicyMakers : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.WitUses</class-id> <category>probes</category>

<body package="WitUses">sumInitRess_rOfPlots
 "return the sum value of Plot initRess_r to be recorded" 
^self sumOfAttribute: #initRess_r of: Plot</body>

<body package="WitUses">varianceCapitalOfUsers
 "return the variance value of User capital to be recorded" 
^self varianceOfAttribute: #capital of: User</body>

<body package="WitUses">sumRemainRess_rOfPlots
 "return the sum value of Plot remainRess_r to be recorded" 
^self sumOfAttribute: #remainRess_r of: Plot</body>

<body package="WitUses">sumMyLivestockOfUsers
 "return the sum value of User myLivestock to be recorded" 
^self sumOfAttribute: #myLivestock of: User</body>

<body package="WitUses">sumRemainRessOfRessProviders
 "return the sum value of RessProvider remainRess to be recorded" 
^self sumOfAttribute: #remainRess of: RessProvider</body>

<body package="WitUses">sumCapitalOfUsers
 "return the sum value of User capital to be recorded" 
^self sumOfAttribute: #capital of: User</body>

<body package="WitUses">sumSeasonTappedRess_rOfUsers
 "return the sum value of User seasonTappedRess_r to be recorded" 
^self sumOfAttribute: #seasonTappedRess_r of: User</body>

<body package="WitUses">sumYearlyTappedRessRsOfUsers
 "return the sum value of User yearlyTappedRessRs to be recorded" 
^self sumOfAttribute: #yearlyTappedRessRs of: User</body>

<body package="WitUses">sumSeasonTappedRessSrOfUsers
 "return the sum value of User seasonTappedRessSr to be recorded" 
^self sumOfAttribute: #seasonTappedRessSr of: User</body>

<body package="WitUses">sumRemainRessSrOfPlots
 "return the sum value of Plot remainRessSr to be recorded" 
^self sumOfAttribute: #remainRessSr of: Plot</body>

<body package="WitUses">sumSeasonTappedRessRS_SettledOfUsers
 "return the sum value of User seasonTappedRessRS_Settled to be recorded" 
^self sumOfAttribute: #seasonTappedRessRS_Settled of: User</body>

<body package="WitUses">sumRemainRessRSOfPlots
 "return the sum value of Plot remainRessRS to be recorded" 
^self sumOfAttribute: #remainRessRS of: Plot</body>

<body package="WitUses">sumInitRessRsOfPlots
 "return the sum value of Plot initRessRs to be recorded" 
^self sumOfAttribute: #initRessRs of: Plot</body>

<body package="WitUses">sumInitRessRSOfPlots
 "return the sum value of Plot initRessRS to be recorded" 
^self sumOfAttribute: #initRessRS of: Plot</body>

<body package="WitUses">sumSeasonTappedRessRS_MobileOfUsers
 "return the sum value of User seasonTappedRessRS_Mobile to be recorded" 
^self sumOfAttribute: #seasonTappedRessRS_Mobile of: User</body>

<body package="WitUses">sumSeasonTappedRessRsOfUsers
 "return the sum value of User seasonTappedRessRs to be recorded" 
^self sumOfAttribute: #seasonTappedRessRs of: User</body>

<body package="WitUses">sumInitRessSrOfPlots
 "return the sum value of Plot initRessSr to be recorded" 
^self sumOfAttribute: #initRessSr of: Plot</body>

<body package="WitUses">sumRemainRessRsOfPlots
 "return the sum value of Plot remainRessRs to be recorded" 
^self sumOfAttribute: #remainRessRs of: Plot</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.WitUses</class-id> <category>accessing</category>

<body package="WitUses">theRessProviders
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theRessProviders ifNil:[theRessProviders := IndexedSet new]</body>

<body package="WitUses">theNorthEasts
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theNorthEasts ifNil:[theNorthEasts := IndexedSet new]</body>

<body package="WitUses">theOveralls: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theOveralls := aNewCollection</body>

<body package="WitUses">theEnVariabilitys
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theEnVariabilitys ifNil:[theEnVariabilitys := IndexedSet new]</body>

<body package="WitUses">numOfMobileUses: aSmallInteger 
	"Setter accessor of attribute numOfMobileUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfMobileUses := aSmallInteger</body>

<body package="WitUses">theUses: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theUses := aNewCollection</body>

<body package="WitUses">numOfOveralls
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfOveralls ifNil:[numOfOveralls := 1]</body>

<body package="WitUses">theOveralls
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theOveralls ifNil:[theOveralls := IndexedSet new]</body>

<body package="WitUses">numOfOveralls: aSmallInteger 
	"Setter accessor of attribute numOfOveralls	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfOveralls := aSmallInteger</body>

<body package="WitUses">thePolicyMakers: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	thePolicyMakers := aNewCollection</body>

<body package="WitUses">thePickUses
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^thePickUses ifNil:[thePickUses := IndexedSet new]</body>

<body package="WitUses">numOfNorthEasts
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfNorthEasts ifNil:[numOfNorthEasts := 1]</body>

<body package="WitUses">theSeasonRessProviderUses
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theSeasonRessProviderUses ifNil:[theSeasonRessProviderUses := IndexedSet new]</body>

<body package="WitUses">theUses
	"Returns a collection of all the instances of aClass (and sub classes) collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "

   ^self allTheEntities: Use</body>

<body package="WitUses">theMobileUses: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theMobileUses := aNewCollection</body>

<body package="WitUses">theSouthEasts
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theSouthEasts ifNil:[theSouthEasts := IndexedSet new]</body>

<body package="WitUses">numOfMobileUses
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfMobileUses ifNil:[numOfMobileUses := 1]</body>

<body package="WitUses">theRulesImplementers
	"Returns a collection of all the instances of aClass (and sub classes) collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "

   ^self allTheEntities: RulesImplementer</body>

<body package="WitUses">theIntensiveUses: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theIntensiveUses := aNewCollection</body>

<body package="WitUses">theUsers: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theUsers := aNewCollection</body>

<body package="WitUses">numOfMonitorings
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfMonitorings ifNil:[numOfMonitorings := 1]</body>

<body package="WitUses">numOfGlobals
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfGlobals ifNil:[numOfGlobals := 1]</body>

<body package="WitUses">theNorthEasts: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theNorthEasts := aNewCollection</body>

<body package="WitUses">numOfUsers: aSmallInteger 
	"Setter accessor of attribute numOfUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfUsers := aSmallInteger</body>

<body package="WitUses">numOfSimulationParameterss
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfSimulationParameterss ifNil:[numOfSimulationParameterss := 1]</body>

<body package="WitUses">theIntensiveUses
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theIntensiveUses ifNil:[theIntensiveUses := IndexedSet new]</body>

<body package="WitUses">theSeasonPlotUses
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theSeasonPlotUses ifNil:[theSeasonPlotUses := IndexedSet new]</body>

<body package="WitUses">numOfSouthEasts
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfSouthEasts ifNil:[numOfSouthEasts := 1]</body>

<body package="WitUses">theCalibrations
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theCalibrations ifNil:[theCalibrations := IndexedSet new]</body>

<body package="WitUses">numOfPolicyMakers
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfPolicyMakers ifNil:[numOfPolicyMakers := 1]</body>

<body package="WitUses">theRessProviders: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theRessProviders := aNewCollection</body>

<body package="WitUses">numOfGlobals: aSmallInteger 
	"Setter accessor of attribute numOfGlobals	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfGlobals := aSmallInteger</body>

<body package="WitUses">theRulesImplementers: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theRulesImplementers := aNewCollection</body>

<body package="WitUses">numOfIntensiveUses
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfIntensiveUses ifNil:[numOfIntensiveUses := 1]</body>

<body package="WitUses">theUsers
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theUsers ifNil:[theUsers := IndexedSet new]</body>

<body package="WitUses">numOfSouthWests: aSmallInteger 
	"Setter accessor of attribute numOfSouthWests	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfSouthWests := aSmallInteger</body>

<body package="WitUses">numOfSouthWests
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfSouthWests ifNil:[numOfSouthWests := 1]</body>

<body package="WitUses">numOfPickUses: aSmallInteger 
	"Setter accessor of attribute numOfPickUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfPickUses := aSmallInteger</body>

<body package="WitUses">numOfMonitorings: aSmallInteger 
	"Setter accessor of attribute numOfMonitorings	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfMonitorings := aSmallInteger</body>

<body package="WitUses">thePlots: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	thePlots := aNewCollection</body>

<body package="WitUses">theEnVariabilitys: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theEnVariabilitys := aNewCollection</body>

<body package="WitUses">theSouthWests: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theSouthWests := aNewCollection</body>

<body package="WitUses">numOfSimulationParameterss: aSmallInteger 
	"Setter accessor of attribute numOfSimulationParameterss	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfSimulationParameterss := aSmallInteger</body>

<body package="WitUses">theMobileUses
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theMobileUses ifNil:[theMobileUses := IndexedSet new]</body>

<body package="WitUses">numOfPickUses
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfPickUses ifNil:[numOfPickUses := 1]</body>

<body package="WitUses">numOfPolicyMakers: aSmallInteger 
	"Setter accessor of attribute numOfPolicyMakers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfPolicyMakers := aSmallInteger</body>

<body package="WitUses">thePlots
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^thePlots ifNil:[thePlots := IndexedSet new]</body>

<body package="WitUses">theCalibrations: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theCalibrations := aNewCollection</body>

<body package="WitUses">numOfSetlledUses: aSmallInteger 
	"Setter accessor of attribute numOfSetlledUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfSetlledUses := aSmallInteger</body>

<body package="WitUses">thePickUses: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	thePickUses := aNewCollection</body>

<body package="WitUses">numOfIntensiveUses: aSmallInteger 
	"Setter accessor of attribute numOfIntensiveUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfIntensiveUses := aSmallInteger</body>

<body package="WitUses">theSettledUses: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theSettledUses := aNewCollection</body>

<body package="WitUses">theSouthWests
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theSouthWests ifNil:[theSouthWests := IndexedSet new]</body>

<body package="WitUses">theSeasonPlotUses: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theSeasonPlotUses := aNewCollection</body>

<body package="WitUses">numOfSouthEasts: aSmallInteger 
	"Setter accessor of attribute numOfSouthEasts	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfSouthEasts := aSmallInteger</body>

<body package="WitUses">theNorthWests
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theNorthWests ifNil:[theNorthWests := IndexedSet new]</body>

<body package="WitUses">theSettledUses
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^theSettledUses ifNil:[theSettledUses := IndexedSet new]</body>

<body package="WitUses">theSouthEasts: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theSouthEasts := aNewCollection</body>

<body package="WitUses">numOfNorthWests
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfNorthWests ifNil:[numOfNorthWests := 1]</body>

<body package="WitUses">theNorthWests: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theNorthWests := aNewCollection</body>

<body package="WitUses">theSeasonRessProviderUses: aNewCollection
	"Set a new collection for theXXXs attribute.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	theSeasonRessProviderUses := aNewCollection</body>

<body package="WitUses">numOfUsers
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfUsers ifNil:[numOfUsers := 1]</body>

<body package="WitUses">numOfNorthEasts: aSmallInteger 
	"Setter accessor of attribute numOfNorthEasts	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfNorthEasts := aSmallInteger</body>

<body package="WitUses">numOfNorthWests: aSmallInteger 
	"Setter accessor of attribute numOfNorthWests	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	numOfNorthWests := aSmallInteger</body>

<body package="WitUses">numOfSetlledUses
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^numOfSetlledUses ifNil:[numOfSetlledUses := 1]</body>

<body package="WitUses">thePolicyMakers
	"Returns a collection of all the instances of the sub classes collected by cormasModel.
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"


^thePolicyMakers ifNil:[thePolicyMakers := IndexedSet new]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.WitUses</class-id> <category>init</category>

<body package="WitUses">initRulesImplementers

	self newEntity: Overall.
	self newEntity:  NorthWest.
	self newEntity:  NorthEast.
	self newEntity:  SouthWest.
	self newEntity:  SouthEast.

	self thePlots do: [:p | Overall myTerritory add: p].
	self thePlots do: [:p | ((p x &lt;= 6) and: [p y &lt;= 6]) ifTrue: [NorthWest myTerritory add: p]].
	self thePlots do: [:p | ((p x &lt;= 6) and: [p y &gt; 6]) ifTrue: [SouthWest myTerritory add: p]].
	self thePlots do: [:p | ((p x &gt; 6) and: [p y &lt;= 6]) ifTrue: [NorthEast myTerritory add: p]].
	self thePlots do: [:p | ((p x &gt; 6) and: [p y &gt; 6]) ifTrue: [SouthEast myTerritory add: p]].

	self theRulesImplementers do: [:r | r mySeasonPlotUses: (self theSeasonPlotUses select: [:spu | r class myTerritory contains: [:p | p =spu hostedPlot]])].
	self theRulesImplementers do: [:r | r myRessProviders: (self theRessProviders select: [:rp | r class myTerritory includes: rp patch])].
	self theRulesImplementers do: [:r | r mySeasonRessProviderUses: (self theSeasonRessProviderUses select: [:srpu | r class myTerritory contains: [:p | p =srpu hostedPlot]])].
	self theRulesImplementers do: [:r | r myUsers: self theUsers].
	self theRulesImplementers do: [:r | (r isKindOf: Overall) ifTrue: [r subRulesImplementers:self theRulesImplementers. r subRulesImplementers remove: r]].</body>

<body package="WitUses">initRessProviders

	self createN: (EnVariability nberSteadyRessProv + EnVariability nberTempRessProv) randomlyLocatedAloneEntities: RessProvider.
	
(Cormas selectRandomlyFrom: self theRessProviders subCollectionSize: EnVariability nberTempRessProv constrainedBy: [:rp | rp providerType isNil]) do: [:rp | rp providerType: '1'].
(Cormas selectRandomlyFrom: self theRessProviders subCollectionSize: EnVariability nberSteadyRessProv constrainedBy: [:rp | rp providerType isNil]) do: [:rp | rp providerType: '2'].</body>

<body package="WitUses">initCalibration

	self createN: 1 entity: Calibration.</body>

<body package="WitUses">initUsers
	
self createN: EnVariability usersNber randomlyLocatedAloneEntities: User.
self theUsers do: [:u | u myTerritory: self thePlots].
self theUsers do: [:u | u allUsers: self theUsers].
self theUsers do: [:u | u allSeasonPlotUses: self theSeasonPlotUses].
self theUsers do: [:u | u allRessProviders: self theRessProviders].
self theUsers do: [:u | u allRulesImplementers: self theRulesImplementers].
self theUsers do: [:u | u initSocialGroup].
self theUsers do: [:u | u initCapital].
self theUsers do: [:u | self theSeasonPlotUses do: [:spu | (spu hostedPlot = u patch) ifTrue: [spu pastUsers add: u]]].</body>

<body package="WitUses">initUses
	
self newEntity: SettledUse.
self newEntity:  MobileUse.
self newEntity:  PickUse.
self newEntity:  IntensiveUse.</body>

<body package="WitUses">initSeasonRessProviderUses
| srp |

	self theRessProviders
		do:
			[:rp | 
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '1'.
			srp activity: 'Settled'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '1'.
			srp activity: 'Mobile'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '1'.
			srp activity: 'Pick'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '1'.
			srp activity: 'Intensive'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '2'.
			srp activity: 'Settled'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '2'.
			srp activity: 'Mobile'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '2'.
			srp activity: 'Pick'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '1'.
			srp activity: 'Intensive'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Settled'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Mobile'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Pick'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Intensive'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Settled'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Mobile'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Pick'.
			srp hostedPlot: rp patch.
			srp := self newEntity: SeasonRessProviderUse.
			srp season: '3'.
			srp activity: 'Intensive'.
			srp hostedPlot: rp patch.
			].</body>

<body package="WitUses">initEnVariability
	
	self createN: 1 entity: EnVariability.
	self theEnVariabilitys do: [:e | e myTerritory: self thePlots].
	self theEnVariabilitys do: [:e | e mySeasonPlotUses: self theSeasonPlotUses].
	self theEnVariabilitys do: [:e | e initLandscapes].
	self theEnVariabilitys do: [:e | e myRessProviders: self theRessProviders].
	self theEnVariabilitys do: [:e | e initRess].</body>

<body package="WitUses">initSeasonPlotUse
	
	| spu |
	self thePlots
		do:
			[:p | 
			spu := self newEntity: SeasonPlotUse.
			spu season: '1'.
			spu use: 'Settled'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '1'.
			spu use: 'Mobile'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '1'.
			spu use: 'Picking'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '1'.
			spu use: 'Intensive'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '2'.
			spu use: 'Settled'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '2'.
			spu use: 'Mobile'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '2'.
			spu use: 'Picking'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '2'.
			spu use: 'Intensive'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '3'.
			spu use: 'Settled'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '3'.
			spu use: 'Mobile'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '3'.
			spu use: 'Picking'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '3'.
			spu use: 'Intensive'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '4'.
			spu use: 'Settled'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '4'.
			spu use: 'Mobile'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '4'.
			spu use: 'Picking'.
			spu hostedPlot: p.
			spu := self newEntity: SeasonPlotUse.
			spu season: '4'.
			spu use: 'Intensive'.
			spu hostedPlot: p].</body>

<body package="WitUses">initPolicyMaker
	
	self createN: 1 entity: PolicyMaker.
	self thePolicyMakers do: [:pp |  pp allRulesImplementers: self theRulesImplementers].
	self thePolicyMakers do: [:pp |  pp allPlots: self thePlots].
	self thePolicyMakers do: [:pp |  pp allSeasonPlotUses: self theSeasonPlotUses].
	self thePolicyMakers do: [:pp |  pp allUsers: self theUsers].</body>

<body package="WitUses">init


	self createGridX: 12 Y: 12 neighbourhood: 4 closed: false.
	self initUses.
	self initCalibration.
	self initSeasonPlotUse.
	self initRessProviders.
	self initEnVariability.
	self initSeasonRessProviderUses.
	self initUsers.
	self initRulesImplementers.
	self initPolicyMaker.</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.WitUses</class-id> <category>default values</category>

<body package="WitUses">hiddenAttributes
"returns a list of hidden attributs that are not displayed by the parameter interface.
This method is automatically generated by the Parameters interface."
	| aList cav |
	aList := List new.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfUsers value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfSetlledUses value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfMobileUses value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfPickUses value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfIntensiveUses value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfSimulationParameterss value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfMonitorings value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfGlobals value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfNorthWests value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfSouthWests value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfNorthEasts value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfSouthEasts value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfOveralls value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: WitUses attribute: #numOfPolicyMakers value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #remainRessRS value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #remainRessSr value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #remainRess_r value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #remainRess_s value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #remainRessRs value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #initRessRS value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #initRessRs value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #initRessSr value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #initRess_r value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #initRess_s value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #overUseRs value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #overUseSr value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #overUse_r value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #memoOverUse_r value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #memoOverUseSr value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #memoOverUseRs value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #closureDuration value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Plot attribute: #zoning value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Overall attribute: #landTenureRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Overall attribute: #landUsesRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Overall attribute: #envRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Overall attribute: #socialRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: Overall attribute: #sahelianRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthWest attribute: #landTenureRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthWest attribute: #landUsesRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthWest attribute: #envRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthWest attribute: #socialRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthWest attribute: #sahelianRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthWest attribute: #landTenureRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthWest attribute: #landUsesRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthWest attribute: #envRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthWest attribute: #socialRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthWest attribute: #sahelianRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #socialGroup value: ' '.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #capital value: 1.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #myLivestock value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #myHungryLivestock value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #yearlyTappedRessRs value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #seasonTappedRess_r value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #seasonTappedRessSr value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #seasonTappedRessRS_Settled value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #seasonTappedRessRS_Mobile value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: User attribute: #seasonTappedRessRs value: 0.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthEast attribute: #landTenureRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthEast attribute: #landUsesRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthEast attribute: #envRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthEast attribute: #socialRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: NorthEast attribute: #sahelianRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthEast attribute: #landTenureRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthEast attribute: #landUsesRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthEast attribute: #envRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthEast attribute: #socialRule value: ''.
	cav isHidden: true.
	aList add: cav.
	cav := ClassAttributeValue newClass: SouthEast attribute: #sahelianRule value: ''.
	cav isHidden: true.
	aList add: cav.
	^aList</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.WitUses</class-id> <category>control</category>

<body package="WitUses">step: t

"spécification des règles appliquées"
self thePolicyMakers do: [: pp | pp decidingPolicies].
"Si d'ébut d'une période d'application d'un policy, remise à zéro (reset) des rules settings"
(PolicyMaker policyDuration isNil) ifFalse: [self thePolicyMakers do: [: pp | pp resetRulesSettings]]
"si pas de policy, mise à jour tout de même de l'assise territoriale des users (cf. règle localist)"
 ifTrue: [self theUsers do: [:u | u myTerritory: self thePlots]].

"mise en oeuvre des nouvelles règles"
self theRulesImplementers do: [:r | r applyingRules].

"renouvellement annuel des ressources"
self theEnVariabilitys do: [:e | e ressRenewing]. 
"Puis, les users se lancent dans les activités, en fonction de l'ordre de priorité permis par les RulesImplementers"
"self theRulesImplementers do: [:r | r openingUsing]." 
self theUsers do: [:u | u implementUses].

"Puis à la fin de chaque année,chaque user ponctionne de son capital ses livingStandard besoins" 
((WitUses timeStep\\4)=0) ifTrue: [self theCalibrations do: [:c | self theUsers do: [:u | u capital: u capital - Calibration livingStandard]]].</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SeasonRessProviderUse class</class-id> <category>+ parameter unit</category>

<body package="WitUses">currentUsers_type 
	"Return the type of attribute #currentUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">hostedPlot_type 
	"Return the type of attribute #hostedPlot.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">pastUsers_type 
	"Return the type of attribute #pastUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">season_type 
	"Return the type of attribute #season.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Array</body>

<body package="WitUses">use_type 
	"Return the type of attribute #use.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Array</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SeasonRessProviderUse class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SeasonRessProviderUse class</class-id> <category>accessing</category>

<body package="WitUses">pastUsers: aOrderedCollection 
	"Setter accessor of attribute pastUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	pastUsers = aOrderedCollection ifTrue:[^nil].   "for optimization"
	pastUsers := aOrderedCollection. 
	self changed</body>

<body package="WitUses">pastUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^pastUsers ifNil:[pastUsers := OrderedCollection new]</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SeasonRessProviderUse class</class-id> <category>- default value</category>

<body package="WitUses">currentUsers_default 
	"Return the default value of attribute #currentUsers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">hostedPlot_default 
	"Return the default value of attribute #hostedPlot : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">use_default 
	"Return the default value of attribute #use : #('Picking' 'Settled' 'Mobile' 'Intensive').	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^#('Picking' 'Settled' 'Mobile' 'Intensive')</body>

<body package="WitUses">pastUsers_default 
	"Return the default value of attribute #pastUsers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">season_default 
	"Return the default value of attribute #season : #(1 2 3 4).	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^#(1 2 3 4)</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SeasonRessProviderUse</class-id> <category>accessing</category>

<body package="WitUses">currentUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^currentUsers ifNil:[currentUsers := OrderedCollection new]</body>

<body package="WitUses">hostedPlot
	"Getter accessor without initial value "
	^hostedPlot</body>

<body package="WitUses">season
	"Getter accessor with initial value = #(1 2 3 4). 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^season ifNil:[season := #(1 2 3 4)]</body>

<body package="WitUses">hostedPlot: aUndefinedObject 
	"Setter accessor of attribute hostedPlot	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	hostedPlot := aUndefinedObject</body>

<body package="WitUses">currentUsers: aOrderedCollection 
	"Setter accessor of attribute currentUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	currentUsers := aOrderedCollection</body>

<body package="WitUses">activity: aArray 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use := aArray</body>

<body package="WitUses">use: aArray 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use := aArray</body>

<body package="WitUses">season: aArray 
	"Setter accessor of attribute season	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	season := aArray</body>

<body package="WitUses">activity
	"Getter accessor with initial value = #('Settled' 'Mobile' 'Pick' 'Intensive'). 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := #('Settled' 'Mobile' 'Pick' 'Intensive')]</body>

<body package="WitUses">use
	"Getter accessor with initial value = #('Picking' 'Settled' 'Mobile' 'Intensive'). 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := #('Picking' 'Settled' 'Mobile' 'Intensive')]</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.IntensiveUse class</class-id> <category>+ parameter unit</category>

<body package="WitUses">use_type 
	"Return the type of attribute #use.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">incomes_type 
	"Return the type of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">incomes_unit 
	"Return the unit of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.IntensiveUse class</class-id> <category>accessing</category>

<body package="WitUses">use
	"Getter accessor with initial value = 'Intensive'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := 'Intensive']</body>

<body package="WitUses">overYearlyThresholdRess
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overYearlyThresholdRess ifNil:[overYearlyThresholdRess := 0]</body>

<body package="WitUses">ressTapping
	"Getter accessor with initial value = 10. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressTapping ifNil:[ressTapping := 10]</body>

<body package="WitUses">resilienceRatioRess
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^resilienceRatioRess ifNil:[resilienceRatioRess := 0]</body>

<body package="WitUses">myRadius
	"Getter accessor without initial value "
	^myRadius</body>

<body package="WitUses">incomesRate
	"Getter accessor without initial value "
	^incomesRate</body>

<body package="WitUses">use: aByteString 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use = aByteString ifTrue:[^nil].   "for optimization"
	use := aByteString. 
	self changed</body>

<body package="WitUses">overRessUseRatio
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overRessUseRatio ifNil:[overRessUseRatio := 0]</body>

<body package="WitUses">ressTapping: aSmallInteger 
	"Setter accessor of attribute incomes	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressTapping := aSmallInteger</body>

<body package="WitUses">needRS
	"Getter accessor without initial value "
	^needRS</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.IntensiveUse class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.IntensiveUse class</class-id> <category>- default value</category>

<body package="WitUses">use_default 
	"Return the default value of attribute #use : 'Intensive'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'Intensive'</body>

<body package="WitUses">incomes_default 
	"Return the default value of attribute #incomes : 10.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^10</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.SettledUse class</class-id> <category>+ parameter unit</category>

<body package="WitUses">incomes_unit 
	"Return the unit of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">requiredCapital_unit 
	"Return the unit of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">incomesRate_type 
	"Return the type of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">needRS_unit 
	"Return the unit of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">resilienceRatioRess_type 
	"Return the type of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">requiredInvest_unit 
	"Return the unit of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overRessUseRatio_unit 
	"Return the unit of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overRessUseRatio_type 
	"Return the type of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">incomesRate_unit 
	"Return the unit of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overYearlyThresholdRess_unit 
	"Return the unit of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressTapping_type 
	"Return the type of attribute #ressTapping.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">overYearlyThresholdRess_type 
	"Return the type of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">needRS_type 
	"Return the type of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">myRadius_unit 
	"Return the unit of attribute #myRadius.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">requiredCapital_type 
	"Return the type of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">use_type 
	"Return the type of attribute #use.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">requiredInvest_type 
	"Return the type of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">incomes_type 
	"Return the type of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">ressTapping_unit 
	"Return the unit of attribute #ressTapping.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">myRadius_type 
	"Return the type of attribute #myRadius.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">resilienceRatioRess_unit 
	"Return the unit of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SettledUse class</class-id> <category>accessing</category>

<body package="WitUses">myRadius
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myRadius ifNil:[myRadius := 0]</body>

<body package="WitUses">overRessUseRatio: aFloat 
	"Setter accessor of attribute overRessUseRatio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overRessUseRatio := aFloat</body>

<body package="WitUses">requiredCapital
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^requiredCapital ifNil:[requiredCapital := 0]</body>

<body package="WitUses">ressTapping
	"Getter accessor with initial value = 0.5. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressTapping ifNil:[ressTapping := 0.5]</body>

<body package="WitUses">overRessUseRatio
	"Getter accessor with initial value = 0.2. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overRessUseRatio ifNil:[overRessUseRatio := 0.2]</body>

<body package="WitUses">use
	"Getter accessor with initial value = 'Settled'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := 'Settled']</body>

<body package="WitUses">resilienceRatioRess: aFloat 
	"Setter accessor of attribute resilienceRatioRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	resilienceRatioRess := aFloat</body>

<body package="WitUses">resilienceRatioRess
	"Getter accessor with initial value = 8.5. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^resilienceRatioRess ifNil:[resilienceRatioRess := 8.5]</body>

<body package="WitUses">incomesRate
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^incomesRate ifNil:[incomesRate := 1]</body>

<body package="WitUses">ressTapping: aFloat 
	"Setter accessor of attribute ressTapping	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressTapping := aFloat</body>

<body package="WitUses">requiredInvest
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^requiredInvest ifNil:[requiredInvest := 0]</body>

<body package="WitUses">overYearlyThresholdRess
	"Getter accessor with initial value = 2. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overYearlyThresholdRess ifNil:[overYearlyThresholdRess := 2]</body>

<body package="WitUses">needRS
	"Getter accessor with initial value = 0.4. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^needRS ifNil:[needRS := 0.4]</body>

<body package="WitUses">myRadius: aSmallInteger 
	"Setter accessor of attribute myRadius	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myRadius = aSmallInteger ifTrue:[^nil].   "for optimization"
	myRadius := aSmallInteger. 
	self changed</body>

<body package="WitUses">requiredInvest: aSmallInteger 
	"Setter accessor of attribute requiredInvest	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredInvest := aSmallInteger</body>

<body package="WitUses">requiredCapital: aSmallInteger 
	"Setter accessor of attribute requiredCapital	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredCapital := aSmallInteger</body>

<body package="WitUses">incomesRate: aSmallInteger 
	"Setter accessor of attribute incomesRate	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	incomesRate := aSmallInteger</body>

<body package="WitUses">overYearlyThresholdRess: aSmallInteger 
	"Setter accessor of attribute overYearlyThresholdRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overYearlyThresholdRess := aSmallInteger</body>

<body package="WitUses">use: aByteString 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use = aByteString ifTrue:[^nil].   "for optimization"
	use := aByteString. 
	self changed</body>

<body package="WitUses">needRS: aFloat 
	"Setter accessor of attribute needRS	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	needRS := aFloat</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SettledUse class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.SettledUse class</class-id> <category>- default value</category>

<body package="WitUses">overRessUseRatio_default 
	"Return the default value of attribute #overRessUseRatio : 0.2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.2</body>

<body package="WitUses">ressTapping_default 
	"Return the default value of attribute #ressTapping : 0.5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.5</body>

<body package="WitUses">requiredInvest_default 
	"Return the default value of attribute #requiredInvest : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">use_default 
	"Return the default value of attribute #use : 'Settled'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'Settled'</body>

<body package="WitUses">myRadius_default 
	"Return the default value of attribute #myRadius : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">needRS_default 
	"Return the default value of attribute #needRS : 0.4.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.4</body>

<body package="WitUses">incomes_default 
	"Return the default value of attribute #incomes : 2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^2</body>

<body package="WitUses">overYearlyThresholdRess_default 
	"Return the default value of attribute #overYearlyThresholdRess : 2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^2</body>

<body package="WitUses">resilienceRatioRess_default 
	"Return the default value of attribute #resilienceRatioRess : 8.5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^8.5</body>

<body package="WitUses">requiredCapital_default 
	"Return the default value of attribute #requiredCapital : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">incomesRate_default 
	"Return the default value of attribute #incomesRate : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.MobileUse class</class-id> <category>+ parameter unit</category>

<body package="WitUses">resilienceRatioRess_unit 
	"Return the unit of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">resource_type 
	"Return the type of attribute #resource.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">incomes_unit 
	"Return the unit of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">requiredCapital_unit 
	"Return the unit of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">incomesRate_type 
	"Return the type of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">needRS_unit 
	"Return the unit of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">resilienceRatioRess_type 
	"Return the type of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">requiredInvest_unit 
	"Return the unit of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overRessUseRatio_unit 
	"Return the unit of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overRessUseRatio_type 
	"Return the type of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">incomesRate_unit 
	"Return the unit of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overYearlyThresholdRess_unit 
	"Return the unit of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">ressTapping_type 
	"Return the type of attribute #ressTapping.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">overYearlyThresholdRess_type 
	"Return the type of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">coeffMinYearlyTappRess_type 
	"Return the type of attribute #coeffMinYearlyTappRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">myRadius_unit 
	"Return the unit of attribute #myRadius.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">needRS_type 
	"Return the type of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">requiredCapital_type 
	"Return the type of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">use_type 
	"Return the type of attribute #use.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">requiredInvest_type 
	"Return the type of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">incomes_type 
	"Return the type of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^Float</body>

<body package="WitUses">ressTapping_unit 
	"Return the unit of attribute #ressTapping.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">coeffMinYearlyTappRess_unit 
	"Return the unit of attribute #coeffMinYearlyTappRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">myRadius_type 
	"Return the type of attribute #myRadius.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.MobileUse class</class-id> <category>accessing</category>

<body package="WitUses">use: aByteString 
	"Setter accessor of attribute use	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	use = aByteString ifTrue:[^nil].   "for optimization"
	use := aByteString. 
	self changed</body>

<body package="WitUses">needRS: aFloat 
	"Setter accessor of attribute needRS	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	needRS := aFloat</body>

<body package="WitUses">myRadius
	"Getter accessor with initial value = 2. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^myRadius ifNil:[myRadius := 2]</body>

<body package="WitUses">coeffMinYearlyTappRess
	"Getter accessor with initial value = 0.75. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^coeffMinYearlyTappRess ifNil:[coeffMinYearlyTappRess := 0.75]</body>

<body package="WitUses">overRessUseRatio: aFloat 
	"Setter accessor of attribute overRessUseRatio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overRessUseRatio := aFloat</body>

<body package="WitUses">requiredCapital
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^requiredCapital ifNil:[requiredCapital := 0]</body>

<body package="WitUses">ressTapping
	"Getter accessor with initial value = 0.4. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^ressTapping ifNil:[ressTapping := 0.4]</body>

<body package="WitUses">overRessUseRatio
	"Getter accessor with initial value = 0.6. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overRessUseRatio ifNil:[overRessUseRatio := 0.6]</body>

<body package="WitUses">coeffMinYearlyTappRess: aFloat 
	"Setter accessor of attribute coeffMinYearlyTappRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	coeffMinYearlyTappRess := aFloat</body>

<body package="WitUses">resilienceRatioRess: aSmallInteger 
	"Setter accessor of attribute resilienceRatioRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	resilienceRatioRess := aSmallInteger</body>

<body package="WitUses">use
	"Getter accessor with initial value = 'Mobile'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^use ifNil:[use := 'Mobile']</body>

<body package="WitUses">resilienceRatioRess
	"Getter accessor with initial value = 5. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^resilienceRatioRess ifNil:[resilienceRatioRess := 5]</body>

<body package="WitUses">incomesRate
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^incomesRate ifNil:[incomesRate := 1]</body>

<body package="WitUses">resource
	"Getter accessor with initial value = 'Rs'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^resource ifNil:[resource := 'Rs']</body>

<body package="WitUses">ressTapping: aFloat 
	"Setter accessor of attribute ressTapping	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressTapping := aFloat</body>

<body package="WitUses">requiredInvest
	"Getter accessor with initial value = 1. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^requiredInvest ifNil:[requiredInvest := 1]</body>

<body package="WitUses">overYearlyThresholdRess
	"Getter accessor with initial value = 3. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overYearlyThresholdRess ifNil:[overYearlyThresholdRess := 3]</body>

<body package="WitUses">resource: aByteString 
	"Setter accessor of attribute resource	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	resource := aByteString</body>

<body package="WitUses">needRS
	"Getter accessor with initial value = 0.2. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^needRS ifNil:[needRS := 0.2]</body>

<body package="WitUses">myRadius: aSmallInteger 
	"Setter accessor of attribute myRadius	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myRadius = aSmallInteger ifTrue:[^nil].   "for optimization"
	myRadius := aSmallInteger. 
	self changed</body>

<body package="WitUses">requiredInvest: aSmallInteger 
	"Setter accessor of attribute requiredInvest	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredInvest := aSmallInteger</body>

<body package="WitUses">requiredCapital: aSmallInteger 
	"Setter accessor of attribute requiredCapital	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredCapital := aSmallInteger</body>

<body package="WitUses">incomesRate: aSmallInteger 
	"Setter accessor of attribute incomesRate	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	incomesRate := aSmallInteger</body>

<body package="WitUses">overYearlyThresholdRess: aSmallInteger 
	"Setter accessor of attribute overYearlyThresholdRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overYearlyThresholdRess := aSmallInteger</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.MobileUse class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.MobileUse class</class-id> <category>- default value</category>

<body package="WitUses">needRS_default 
	"Return the default value of attribute #needRS : 0.2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.2</body>

<body package="WitUses">coeffMinYearlyTappRess_default 
	"Return the default value of attribute #coeffMinYearlyTappRess : 0.75.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.75</body>

<body package="WitUses">ressTapping_default 
	"Return the default value of attribute #ressTapping : 0.4.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.4</body>

<body package="WitUses">incomesRate_default 
	"Return the default value of attribute #incomesRate : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>

<body package="WitUses">overRessUseRatio_default 
	"Return the default value of attribute #overRessUseRatio : 0.6.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.6</body>

<body package="WitUses">use_default 
	"Return the default value of attribute #use : 'Mobile'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'Mobile'</body>

<body package="WitUses">resilienceRatioRess_default 
	"Return the default value of attribute #resilienceRatioRess : 5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^5</body>

<body package="WitUses">myRadius_default 
	"Return the default value of attribute #myRadius : 2.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^2</body>

<body package="WitUses">resource_default 
	"Return the default value of attribute #resource : 'Rs'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'Rs'</body>

<body package="WitUses">incomes_default 
	"Return the default value of attribute #incomes : 0.5.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0.5</body>

<body package="WitUses">requiredCapital_default 
	"Return the default value of attribute #requiredCapital : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">overYearlyThresholdRess_default 
	"Return the default value of attribute #overYearlyThresholdRess : 3.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^3</body>

<body package="WitUses">requiredInvest_default 
	"Return the default value of attribute #requiredInvest : 1.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^1</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.Use class</class-id> <category>+ parameter unit</category>

<body package="WitUses">myRadius_type 
	"Return the type of attribute #myRadius.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">resilienceRatioRess_unit 
	"Return the unit of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">incomes_unit 
	"Return the unit of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">requiredCapital_unit 
	"Return the unit of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">incomesRate_type 
	"Return the type of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">needRS_unit 
	"Return the unit of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">resilienceRatioRess_type 
	"Return the type of attribute #resilienceRatioRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">requiredInvest_unit 
	"Return the unit of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overRessUseRatio_type 
	"Return the type of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">overRessUseRatio_unit 
	"Return the unit of attribute #overRessUseRatio.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">incomesRate_unit 
	"Return the unit of attribute #incomesRate.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overYearlyThresholdRess_unit 
	"Return the unit of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">overYearlyThresholdRess_type 
	"Return the type of attribute #overYearlyThresholdRess.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^SmallInteger</body>

<body package="WitUses">needRS_type 
	"Return the type of attribute #needRS.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">myRadius_unit 
	"Return the unit of attribute #myRadius.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">requiredCapital_type 
	"Return the type of attribute #requiredCapital.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">requiredInvest_type 
	"Return the type of attribute #requiredInvest.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">incomes_type 
	"Return the type of attribute #incomes.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Use class</class-id> <category>accessing</category>

<body package="WitUses">requiredInvest: aUndefinedObject 
	"Setter accessor of attribute requiredInvest	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredInvest := aUndefinedObject</body>

<body package="WitUses">requiredCapital: aUndefinedObject 
	"Setter accessor of attribute requiredCapital	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	requiredCapital := aUndefinedObject</body>

<body package="WitUses">incomesRate: aUndefinedObject 
	"Setter accessor of attribute incomesRate	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	incomesRate = aUndefinedObject ifTrue:[^nil].   "for optimization"
	incomesRate := aUndefinedObject. 
	self changed</body>

<body package="WitUses">overYearlyThresholdRess: aSmallInteger 
	"Setter accessor of attribute overYearlyThresholdRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overYearlyThresholdRess = aSmallInteger ifTrue:[^nil].   "for optimization"
	overYearlyThresholdRess := aSmallInteger. 
	self changed</body>

<body package="WitUses">needRS: aUndefinedObject 
	"Setter accessor of attribute needRS	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	needRS = aUndefinedObject ifTrue:[^nil].   "for optimization"
	needRS := aUndefinedObject. 
	self changed</body>

<body package="WitUses">myRadius
	"Getter accessor without initial value "
	^myRadius</body>

<body package="WitUses">overRessUseRatio: aSmallInteger 
	"Setter accessor of attribute overRessUseRatio	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overRessUseRatio = aSmallInteger ifTrue:[^nil].   "for optimization"
	overRessUseRatio := aSmallInteger. 
	self changed</body>

<body package="WitUses">requiredCapital
	"Getter accessor without initial value "
	^requiredCapital</body>

<body package="WitUses">ressTapping
	"Getter accessor without initial value "
	^ressTapping</body>

<body package="WitUses">overRessUseRatio
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overRessUseRatio ifNil:[overRessUseRatio := 0]</body>

<body package="WitUses">resilienceRatioRess: aSmallInteger 
	"Setter accessor of attribute resilienceRatioRess	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	resilienceRatioRess = aSmallInteger ifTrue:[^nil].   "for optimization"
	resilienceRatioRess := aSmallInteger. 
	self changed</body>

<body package="WitUses">resilienceRatioRess
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^resilienceRatioRess ifNil:[resilienceRatioRess := 0]</body>

<body package="WitUses">incomesRate
	"Getter accessor without initial value "
	^incomesRate</body>

<body package="WitUses">ressTapping: aUndefinedObject 
	"Setter accessor of attribute incomes	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	ressTapping := aUndefinedObject</body>

<body package="WitUses">requiredInvest
	"Getter accessor without initial value "
	^requiredInvest</body>

<body package="WitUses">overYearlyThresholdRess
	"Getter accessor with initial value = 0. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overYearlyThresholdRess ifNil:[overYearlyThresholdRess := 0]</body>

<body package="WitUses">needRS
	"Getter accessor without initial value "
	^needRS</body>

<body package="WitUses">myRadius: aUndefinedObject 
	"Setter accessor of attribute myRadius	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	myRadius = aUndefinedObject ifTrue:[^nil].   "for optimization"
	myRadius := aUndefinedObject. 
	self changed</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Use class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^true</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Use class</class-id> <category>- default value</category>

<body package="WitUses">needRS_default 
	"Return the default value of attribute #needRS : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">overYearlyThresholdRess_default 
	"Return the default value of attribute #overYearlyThresholdRess : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">incomes_default 
	"Return the default value of attribute #incomes : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">resilienceRatioRess_default 
	"Return the default value of attribute #resilienceRatioRess : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">requiredCapital_default 
	"Return the default value of attribute #requiredCapital : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">incomesRate_default 
	"Return the default value of attribute #incomesRate : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">overRessUseRatio_default 
	"Return the default value of attribute #overRessUseRatio : 0.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^0</body>

<body package="WitUses">ressTapping_default 
	"Return the default value of attribute #ressTapping : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">requiredInvest_default 
	"Return the default value of attribute #requiredInvest : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">myRadius_default 
	"Return the default value of attribute #myRadius : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.Overall class</class-id> <category>+ parameter unit</category>

<body package="WitUses">sahelianRule_type 
	"Return the type of attribute #sahelianRule.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">landTenurePolicy_type 
	"Return the type of attribute #landTenurePolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">socialRule_type 
	"Return the type of attribute #socialRule.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">envRule_type 
	"Return the type of attribute #envRule.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">subRulesImplementers_type 
	"Return the type of attribute #subRulesImplementers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">landTenureRule_type 
	"Return the type of attribute #landTenureRule.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">landUsesPolicy_type 
	"Return the type of attribute #landUsesPolicy.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">landUsesRule_type 
	"Return the type of attribute #landUsesRule.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Overall class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.Overall class</class-id> <category>- default value</category>

<body package="WitUses">subRulesImplementers_default 
	"Return the default value of attribute #subRulesImplementers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">landUsesPolicy_default 
	"Return the default value of attribute #landUsesPolicy : 'firstUser'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'firstUser'</body>

<body package="WitUses">landUsesRule_default 
	"Return the default value of attribute #landUsesRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">socialRule_default 
	"Return the default value of attribute #socialRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">landTenureRule_default 
	"Return the default value of attribute #landTenureRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sahelianRule_default 
	"Return the default value of attribute #sahelianRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">envRule_default 
	"Return the default value of attribute #envRule : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">landTenurePolicy_default 
	"Return the default value of attribute #landTenurePolicy : 'onePlotOneUser'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'onePlotOneUser'</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.Overall</class-id> <category>accessing</category>

<body package="WitUses">envRule: aByteString 
	"Setter accessor of attribute envRule	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	envRule = aByteString ifTrue:[^nil].   "for optimization"
	envRule := aByteString. 
	self changed</body>

<body package="WitUses">sahelianRule: aByteString 
	"Setter accessor of attribute sahelianRule	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sahelianRule = aByteString ifTrue:[^nil].   "for optimization"
	sahelianRule := aByteString. 
	self changed</body>

<body package="WitUses">backUpMyTerritory
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpMyTerritory ifNil:[backUpMyTerritory := OrderedCollection new]</body>

<body package="WitUses">socialRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^socialRule ifNil:[socialRule := '']</body>

<body package="WitUses">landTenureRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landTenureRule ifNil:[landTenureRule := '']</body>

<body package="WitUses">socialRule: aByteString 
	"Setter accessor of attribute socialRule	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	socialRule = aByteString ifTrue:[^nil].   "for optimization"
	socialRule := aByteString. 
	self changed</body>

<body package="WitUses">landUsesRule: aByteString 
	"Setter accessor of attribute landUsesRule	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landUsesRule = aByteString ifTrue:[^nil].   "for optimization"
	landUsesRule := aByteString. 
	self changed</body>

<body package="WitUses">landUsesRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^landUsesRule ifNil:[landUsesRule := '']</body>

<body package="WitUses">landTenureRule: aByteString 
	"Setter accessor of attribute landTenureRule	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	landTenureRule = aByteString ifTrue:[^nil].   "for optimization"
	landTenureRule := aByteString. 
	self changed</body>

<body package="WitUses">sahelianRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sahelianRule ifNil:[sahelianRule := '']</body>

<body package="WitUses">mySeasonRessProviderUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^mySeasonRessProviderUses ifNil:[mySeasonRessProviderUses := OrderedCollection new]</body>

<body package="WitUses">subRulesImplementers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^subRulesImplementers ifNil:[subRulesImplementers := OrderedCollection new]</body>

<body package="WitUses">envRule
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^envRule ifNil:[envRule := '']</body>

<body package="WitUses">backUpOfAllSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpOfAllSeasonPlotUses ifNil:[backUpOfAllSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">backUpAppliedRules
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^backUpAppliedRules ifNil:[backUpAppliedRules := OrderedCollection new]</body>

<body package="WitUses">subRulesImplementers: aOrderedCollection 
	"Setter accessor of attribute subRulesImplementers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	subRulesImplementers = aOrderedCollection ifTrue:[^nil].   "for optimization"
	subRulesImplementers := aOrderedCollection. 
	self changed</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.PolicyMaker class</class-id> <category>+ parameter unit</category>

<body package="WitUses">sWLandUses_type 
	"Return the type of attribute #sWLandUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">sWSahelian_type 
	"Return the type of attribute #sWSahelian.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">overallSocial_type 
	"Return the type of attribute #overallSocial.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">nELandTenure_type 
	"Return the type of attribute #nELandTenure.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">nESahelian_type 
	"Return the type of attribute #nESahelian.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">overallEnv_type 
	"Return the type of attribute #overallEnv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">nWSocial_type 
	"Return the type of attribute #nWSocial.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">overallLandTenure_type 
	"Return the type of attribute #overallLandTenure.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">sELandUses_type 
	"Return the type of attribute #sELandUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">allRulesImplementers_type 
	"Return the type of attribute #allRulesImplementers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">nWSahelian_type 
	"Return the type of attribute #nWSahelian.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">overallLandTenure_unit 
	"Return the unit of attribute #overallLandTenure.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">nWLandTenure_type 
	"Return the type of attribute #nWLandTenure.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">sEEnv_type 
	"Return the type of attribute #sEEnv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">nELandUses_type 
	"Return the type of attribute #nELandUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">nEEnv_type 
	"Return the type of attribute #nEEnv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">sWSocial_type 
	"Return the type of attribute #sWSocial.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">sELandTenure_type 
	"Return the type of attribute #sELandTenure.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">nESocial_type 
	"Return the type of attribute #nESocial.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">policyDuration_type 
	"Return the type of attribute #policyDuration.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^UndefinedObject</body>

<body package="WitUses">nWEnv_type 
	"Return the type of attribute #nWEnv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">policyDuration_unit 
	"Return the unit of attribute #policyDuration.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ ''</body>

<body package="WitUses">sESahelian_type 
	"Return the type of attribute #sESahelian.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">sWLandTenure_type 
	"Return the type of attribute #sWLandTenure.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">allSeasonPlotUses_type 
	"Return the type of attribute #allSeasonPlotUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">allPlots_type 
	"Return the type of attribute #allPlots.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">sWEnv_type 
	"Return the type of attribute #sWEnv.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">allUsers_type 
	"Return the type of attribute #allUsers.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection</body>

<body package="WitUses">sESocial_type 
	"Return the type of attribute #sESocial.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">overallSahelian_type 
	"Return the type of attribute #overallSahelian.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">nWLandUses_type 
	"Return the type of attribute #nWLandUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>

<body package="WitUses">overallLandUses_type 
	"Return the type of attribute #overallLandUses.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^ByteString</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.PolicyMaker class</class-id> <category>testing</category>

<body package="WitUses">isAbstract 
^false</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.PolicyMaker class</class-id> <category>accessing</category>

<body package="WitUses">policyDuration: aUndefinedObject 
	"Setter accessor of attribute policyDuration	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	policyDuration := aUndefinedObject</body>

<body package="WitUses">policyDuration
	"Getter accessor without initial value "
	^policyDuration</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.PolicyMaker class</class-id> <category>- default value</category>

<body package="WitUses">allSeasonPlotUses_default 
	"Return the default value of attribute #allSeasonPlotUses : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">allUsers_default 
	"Return the default value of attribute #allUsers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">nELandTenure_default 
	"Return the default value of attribute #nELandTenure : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">allRulesImplementers_default 
	"Return the default value of attribute #allRulesImplementers : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">nWSocial_default 
	"Return the default value of attribute #nWSocial : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sESahelian_default 
	"Return the default value of attribute #sESahelian : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nEEnv_default 
	"Return the default value of attribute #nEEnv : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nWLandUses_default 
	"Return the default value of attribute #nWLandUses : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nESocial_default 
	"Return the default value of attribute #nESocial : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nWSahelian_default 
	"Return the default value of attribute #nWSahelian : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sWLandUses_default 
	"Return the default value of attribute #sWLandUses : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">allPlots_default 
	"Return the default value of attribute #allPlots : OrderedCollection new.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^OrderedCollection new</body>

<body package="WitUses">sEEnv_default 
	"Return the default value of attribute #sEEnv : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">overallLandTenure_default 
	"Return the default value of attribute #overallLandTenure : 'firstUser'.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^'firstUser'</body>

<body package="WitUses">sELandTenure_default 
	"Return the default value of attribute #sELandTenure : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">overallEnv_default 
	"Return the default value of attribute #overallEnv : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sWEnv_default 
	"Return the default value of attribute #sWEnv : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sWLandTenure_default 
	"Return the default value of attribute #sWLandTenure : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nWLandTenure_default 
	"Return the default value of attribute #nWLandTenure : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sWSahelian_default 
	"Return the default value of attribute #sWSahelian : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sWSocial_default 
	"Return the default value of attribute #sWSocial : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nWEnv_default 
	"Return the default value of attribute #nWEnv : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">overallSocial_default 
	"Return the default value of attribute #overallSocial : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nELandUses_default 
	"Return the default value of attribute #nELandUses : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">overallSahelian_default 
	"Return the default value of attribute #overallSahelian : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">sESocial_default 
	"Return the default value of attribute #sESocial : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">nESahelian_default 
	"Return the default value of attribute #nESahelian : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">policyDuration_default 
	"Return the default value of attribute #policyDuration : nil.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^nil</body>

<body package="WitUses">sELandUses_default 
	"Return the default value of attribute #sELandUses : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>

<body package="WitUses">overallLandUses_default 
	"Return the default value of attribute #overallLandUses : ''.	
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!"

	^''</body>
</methods>


<methods>
<class-id>CormasNS.Models.WitUses.PolicyMaker</class-id> <category>control</category>

<body package="WitUses">decidingPolicies

(self overallLandTenure isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = Overall]) do: [:r | r landTenureRule: self overallLandTenure]].
(self overallLandUses isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = Overall]) do: [:r | r landUsesRule: self overallLandUses]].
(self overallEnv isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = Overall]) do: [:r | r envRule: self overallEnv]].
(self overallSocial isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = Overall]) do: [:r | r socialRule: self overallSocial]].
(self overallSahelian isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = Overall]) do: [:r | r sahelianRule: self overallSahelian]].

(self nWLandTenure isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthWest]) do: [:r | r landTenureRule: self nWLandTenure]].
(self nWLandUses isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthWest]) do: [:r | r landUsesRule: self nWLandUses]].
(self nWEnv isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthWest]) do: [:r | r envRule: self nWEnv]].
(self nWSocial isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthWest]) do: [:r | r socialRule: self nWSocial]].
(self nWSahelian isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthWest]) do: [:r | r sahelianRule: self nWSahelian]].

(self nELandTenure isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthEast]) do: [:r | r landTenureRule: self nELandTenure]].
(self nELandUses isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthEast]) do: [:r | r landUsesRule: self nELandUses]].
(self nEEnv isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthEast]) do: [:r | r envRule: self nEEnv]].
(self nESocial isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthEast]) do: [:r | r socialRule: self nESocial]].
(self nESahelian isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = NorthEast]) do: [:r | r sahelianRule: self nESahelian]].

(self sWLandTenure isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthWest]) do: [:r | r landTenureRule: self sWLandTenure]].
(self sWLandUses isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthWest]) do: [:r | r landUsesRule: self sWLandUses]].
(self sWEnv isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthWest]) do: [:r | r envRule: self sWEnv]].
(self sWSocial isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthWest]) do: [:r | r socialRule: self sWSocial]].
(self sWSahelian isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthWest]) do: [:r | r sahelianRule: self sWSahelian]].

(self sELandTenure isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthEast]) do: [:r | r landTenureRule: self sELandTenure]].
(self sELandUses isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthEast]) do: [:r | r landUsesRule: self sELandUses]].
(self sEEnv isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthEast]) do: [:r | r envRule: self sEEnv]].
(self sESocial isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthEast]) do: [:r | r socialRule: self sESocial]].
(self sESahelian isEmpty) ifFalse: [(self allRulesImplementers select: [:r | r class = SouthEast]) do: [:r | r sahelianRule: self sESahelian]].</body>

<body package="WitUses">resetRulesSettings

	"Si d'ébut d'une période d'application d'un policy, remise à zéro (reset) des des plots et spu accessibles A CHAQUE DEBUT DE PERIODE D'APPLICATION"

(WitUses timeStep=(PolicyMaker policyDuration*4)\\1) ifTrue:[
"remise à zéro des spus"
self allSeasonPlotUses do: [:spu | spu currentUsers removeAll].
"remise à zéro des mises en défens au niveau des plots"
self allPlots do: [:p | (p holder isNil) ifFalse: [p holder:nil]. p closureDuration:0].
"remise à zéro des règles en mémoire dans les users"
self allUsers do: [:u | u hotPlots removeAll. u appliedRules removeAll].
].</body>
</methods>

<methods>
<class-id>CormasNS.Models.WitUses.PolicyMaker</class-id> <category>accessing</category>

<body package="WitUses">sESahelian: aByteString 
	"Setter accessor of attribute sESahelian	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sESahelian = aByteString ifTrue:[^nil].   "for optimization"
	sESahelian := aByteString. 
	self changed</body>

<body package="WitUses">sWLandTenure: aByteString 
	"Setter accessor of attribute sWLandTenure	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sWLandTenure = aByteString ifTrue:[^nil].   "for optimization"
	sWLandTenure := aByteString. 
	self changed</body>

<body package="WitUses">nESahelian
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nESahelian ifNil:[nESahelian := '']</body>

<body package="WitUses">nELandUses: aByteString 
	"Setter accessor of attribute nELandUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nELandUses = aByteString ifTrue:[^nil].   "for optimization"
	nELandUses := aByteString. 
	self changed</body>

<body package="WitUses">nESocial: aByteString 
	"Setter accessor of attribute nESocial	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nESocial = aByteString ifTrue:[^nil].   "for optimization"
	nESocial := aByteString. 
	self changed</body>

<body package="WitUses">sELandUses
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sELandUses ifNil:[sELandUses := '']</body>

<body package="WitUses">sELandUses: aByteString 
	"Setter accessor of attribute sELandUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sELandUses = aByteString ifTrue:[^nil].   "for optimization"
	sELandUses := aByteString. 
	self changed</body>

<body package="WitUses">sESocial: aByteString 
	"Setter accessor of attribute sESocial	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sESocial = aByteString ifTrue:[^nil].   "for optimization"
	sESocial := aByteString. 
	self changed</body>

<body package="WitUses">allRulesImplementers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allRulesImplementers ifNil:[allRulesImplementers := OrderedCollection new]</body>

<body package="WitUses">sWLandUses
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sWLandUses ifNil:[sWLandUses := '']</body>

<body package="WitUses">sWLandUses: aByteString 
	"Setter accessor of attribute sWLandUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sWLandUses = aByteString ifTrue:[^nil].   "for optimization"
	sWLandUses := aByteString. 
	self changed</body>

<body package="WitUses">nEEnv: aByteString 
	"Setter accessor of attribute nEEnv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nEEnv = aByteString ifTrue:[^nil].   "for optimization"
	nEEnv := aByteString. 
	self changed</body>

<body package="WitUses">nWSahelian
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nWSahelian ifNil:[nWSahelian := '']</body>

<body package="WitUses">nWSocial
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nWSocial ifNil:[nWSocial := '']</body>

<body package="WitUses">sWLandTenure
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sWLandTenure ifNil:[sWLandTenure := '']</body>

<body package="WitUses">overallLandUses: aByteString 
	"Setter accessor of attribute overallLandUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overallLandUses = aByteString ifTrue:[^nil].   "for optimization"
	overallLandUses := aByteString. 
	self changed</body>

<body package="WitUses">sESahelian
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sESahelian ifNil:[sESahelian := '']</body>

<body package="WitUses">allUsers
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allUsers ifNil:[allUsers := OrderedCollection new]</body>

<body package="WitUses">sWSocial: aByteString 
	"Setter accessor of attribute sWSocial	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sWSocial = aByteString ifTrue:[^nil].   "for optimization"
	sWSocial := aByteString. 
	self changed</body>

<body package="WitUses">nWEnv: aByteString 
	"Setter accessor of attribute nWEnv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nWEnv = aByteString ifTrue:[^nil].   "for optimization"
	nWEnv := aByteString. 
	self changed</body>

<body package="WitUses">allPlots
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allPlots ifNil:[allPlots := OrderedCollection new]</body>

<body package="WitUses">overallSahelian: aByteString 
	"Setter accessor of attribute overallSahelian	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overallSahelian = aByteString ifTrue:[^nil].   "for optimization"
	overallSahelian := aByteString. 
	self changed</body>

<body package="WitUses">sESocial
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sESocial ifNil:[sESocial := '']</body>

<body package="WitUses">nELandTenure: aByteString 
	"Setter accessor of attribute nELandTenure	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nELandTenure = aByteString ifTrue:[^nil].   "for optimization"
	nELandTenure := aByteString. 
	self changed</body>

<body package="WitUses">overallLandTenure
	"Getter accessor with initial value = 'firstUser'. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overallLandTenure ifNil:[overallLandTenure := 'firstUser']</body>

<body package="WitUses">overallLandTenure: aByteString 
	"Setter accessor of attribute overallLandTenure	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overallLandTenure := aByteString</body>

<body package="WitUses">sWSahelian
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sWSahelian ifNil:[sWSahelian := '']</body>

<body package="WitUses">nELandUses
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nELandUses ifNil:[nELandUses := '']</body>

<body package="WitUses">nWEnv
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nWEnv ifNil:[nWEnv := '']</body>

<body package="WitUses">nESahelian: aByteString 
	"Setter accessor of attribute nESahelian	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nESahelian = aByteString ifTrue:[^nil].   "for optimization"
	nESahelian := aByteString. 
	self changed</body>

<body package="WitUses">sWEnv: aByteString 
	"Setter accessor of attribute sWEnv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sWEnv = aByteString ifTrue:[^nil].   "for optimization"
	sWEnv := aByteString. 
	self changed</body>

<body package="WitUses">nWLandTenure
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nWLandTenure ifNil:[nWLandTenure := '']</body>

<body package="WitUses">nWLandUses
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nWLandUses ifNil:[nWLandUses := '']</body>

<body package="WitUses">allUsers: aOrderedCollection 
	"Setter accessor of attribute allUsers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allUsers = aOrderedCollection ifTrue:[^nil].   "for optimization"
	allUsers := aOrderedCollection. 
	self changed</body>

<body package="WitUses">sWEnv
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sWEnv ifNil:[sWEnv := '']</body>

<body package="WitUses">nELandTenure
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nELandTenure ifNil:[nELandTenure := '']</body>

<body package="WitUses">nWSahelian: aByteString 
	"Setter accessor of attribute nWSahelian	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nWSahelian = aByteString ifTrue:[^nil].   "for optimization"
	nWSahelian := aByteString. 
	self changed</body>

<body package="WitUses">nWLandUses: aByteString 
	"Setter accessor of attribute nWLandUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nWLandUses = aByteString ifTrue:[^nil].   "for optimization"
	nWLandUses := aByteString. 
	self changed</body>

<body package="WitUses">allSeasonPlotUses: aOrderedCollection 
	"Setter accessor of attribute allSeasonPlotUses	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allSeasonPlotUses = aOrderedCollection ifTrue:[^nil].   "for optimization"
	allSeasonPlotUses := aOrderedCollection. 
	self changed</body>

<body package="WitUses">overallSocial: aByteString 
	"Setter accessor of attribute overallSocial	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overallSocial = aByteString ifTrue:[^nil].   "for optimization"
	overallSocial := aByteString. 
	self changed</body>

<body package="WitUses">nEEnv
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nEEnv ifNil:[nEEnv := '']</body>

<body package="WitUses">nWLandTenure: aByteString 
	"Setter accessor of attribute nWLandTenure	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nWLandTenure = aByteString ifTrue:[^nil].   "for optimization"
	nWLandTenure := aByteString. 
	self changed</body>

<body package="WitUses">allSeasonPlotUses
	"Getter accessor with initial value = OrderedCollection new. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^allSeasonPlotUses ifNil:[allSeasonPlotUses := OrderedCollection new]</body>

<body package="WitUses">overallEnv
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overallEnv ifNil:[overallEnv := '']</body>

<body package="WitUses">overallEnv: aByteString 
	"Setter accessor of attribute overallEnv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	overallEnv = aByteString ifTrue:[^nil].   "for optimization"
	overallEnv := aByteString. 
	self changed</body>

<body package="WitUses">overallLandUses
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overallLandUses ifNil:[overallLandUses := '']</body>

<body package="WitUses">overallSahelian
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overallSahelian ifNil:[overallSahelian := '']</body>

<body package="WitUses">sEEnv
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sEEnv ifNil:[sEEnv := '']</body>

<body package="WitUses">sELandTenure: aByteString 
	"Setter accessor of attribute sELandTenure	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sELandTenure = aByteString ifTrue:[^nil].   "for optimization"
	sELandTenure := aByteString. 
	self changed</body>

<body package="WitUses">nWSocial: aByteString 
	"Setter accessor of attribute nWSocial	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	nWSocial = aByteString ifTrue:[^nil].   "for optimization"
	nWSocial := aByteString. 
	self changed</body>

<body package="WitUses">overallSocial
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^overallSocial ifNil:[overallSocial := '']</body>

<body package="WitUses">sELandTenure
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sELandTenure ifNil:[sELandTenure := '']</body>

<body package="WitUses">sWSahelian: aByteString 
	"Setter accessor of attribute sWSahelian	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sWSahelian = aByteString ifTrue:[^nil].   "for optimization"
	sWSahelian := aByteString. 
	self changed</body>

<body package="WitUses">allPlots: aOrderedCollection 
	"Setter accessor of attribute allPlots	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allPlots = aOrderedCollection ifTrue:[^nil].   "for optimization"
	allPlots := aOrderedCollection. 
	self changed</body>

<body package="WitUses">sWSocial
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^sWSocial ifNil:[sWSocial := '']</body>

<body package="WitUses">allRulesImplementers: aOrderedCollection 
	"Setter accessor of attribute allRulesImplementers	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	allRulesImplementers = aOrderedCollection ifTrue:[^nil].   "for optimization"
	allRulesImplementers := aOrderedCollection. 
	self changed</body>

<body package="WitUses">nESocial
	"Getter accessor with initial value = ''. 
	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !! "
	^nESocial ifNil:[nESocial := '']</body>

<body package="WitUses">sEEnv: aByteString 
	"Setter accessor of attribute sEEnv	This method was automatically generated by Cormas.
	!! BECAREFUL, You should not modify this method as it can be generated again when changing the value (Sensitivity Analysis for ex) !!  "
	sEEnv = aByteString ifTrue:[^nil].   "for optimization"
	sEEnv := aByteString. 
	self changed</body>
</methods>




</st-source>
