-- nrs.py

--
--				  NOTICE
--
--    Acquisition, use, and distribution of this module and related
--    materials are subject to the restrictions of a license agreement.
--    Consult the Preface in the User's Manual for the full terms of
--    this agreement.
--
--


NRS
	{
	ccitt
	data(9)
	pss(2342)
	ucl(19200300)
	quipu(99)
	nRSDefinitions(2)
	}

DEFINITIONS
::=

PREFIXES encode decode print

BEGIN

Context [[P struct nrs_info *]]
::=
        INTEGER [[i context]]
        {
            context-unknown(-1) ,
            x29(0) ,
            ts29(1) ,
            niftp(2) ,
            mail-niftp(3) ,
            not-used(4) ,
            mail-telex(5) ,
            jtmp(6) ,
            jtmp-files(7) ,
            jtmp-reg(8) ,
            ybts-node(9) ,
            ybts(10) ,
            ftam(11) ,
            jtm(12) ,
            jtm-reg(13) ,
            vt(14) ,
            motis(15)
        }

Address-space-id
[[P struct nrs_info *]]

::=
        INTEGER [[i addr_sp_id]]
        {
            pss(0) ,
            janet(1) ,
            telex(2) ,
            osi-cons(3)
        }

Route-cost [[P PElement *]]
::=
        ANY [[a *]]

Addressing-info [[P struct addr_info *]]
::=
        CHOICE
            <<addr_info_type>>
        {
        dte-only
		[0] NumericString [[s dte_number]] ,

        dte-appli-info
		[1] SEQUENCE [[T struct addr_info * $ *]]
                {
                dte-number
			[0] NumericString [[s dte_number]] ,

                applic-info
			[1] SEQUENCE OF [[T struct str_seq * $ applic_info ]] <<ss_next>>
                        	VisibleString [[s ss_str]]
                } ,

        dte-cudf
		[2] SEQUENCE [[T struct addr_info * $ *]]
                {
                dte-number
			[0] NumericString [[s dte_number]] ,
                cudf
			[1] OCTET STRING [[s cudf]]
                } ,

        dte-cudf-applic-info
		[3] SEQUENCE [[T struct addr_info * $ *]]
                {
                dte-number
			[0] NumericString [[s dte_number]] ,

                cudf
			[1] OCTET STRING [[s cudf]] ,

                applic-info
			[2] SEQUENCE OF [[T struct str_seq * $ applic_info ]] <<ss_next>>
                        	VisibleString [[s ss_str]]
                } ,

        dte-ybts
		[4] SEQUENCE [[T struct addr_info * $ *]]
                {
                dte-number
			[0] NumericString [[s dte_number]] ,

                ybts-string
			[1] VisibleString [[s ybts_string]]
                } ,

        dte-ybts-applic-info
		[5] SEQUENCE [[T struct addr_info * $ *]]
                {
                dte-number
			[0] NumericString [[s dte_number]] ,

                ybts-string
			[1] VisibleString [[s ybts_string]] ,

                applic-info
			[2] SEQUENCE OF [[T struct str_seq * $ applic_info ]] <<ss_next>>
                        	VisibleString [[s ss_str]]
                } ,

        dte-ybts-appli-relays 
		[6] SEQUENCE [[T struct addr_info * $ *]]
                {
                dte-number
			[0] NumericString [[s dte_number]] ,

                ybts-string
			[1] VisibleString [[s ybts_string]] ,

                applic-relay
			[2] SEQUENCE OF [[T struct str_seq * $ applic_info ]] <<ss_next>>
                        	VisibleString [[s ss_str]]
                } ,

        none-needed
		[7] NULL ,

        osi-addressing
		[8] SEQUENCE [[T struct addr_info * $ *]]
                {
                nsap
			[0] NumericString [[s nsap]] ,

                tselector
			[1] OCTET STRING [[s tselector]]
                            OPTIONAL,

                sselector
			[2] OCTET STRING [[s sselector]]
                            OPTIONAL,

                pselector
			[3] OCTET STRING [[s pselector]]
                            OPTIONAL,

                place-holder
			[4] ANY [[a place_holder]]
                            OPTIONAL,

                application-title
			[5] ANY [[a application_title]]
                            OPTIONAL,

                per-application-context-info
			[6] ANY [[a per_app_context_info]]
                            OPTIONAL
                } ,

        osi-nsap-only
		[9] NumericString [[s nsap]] ,

        osi-nsap-applic-info
		[10] SEQUENCE [[T struct addr_info * $ *]]
                {
                nsap
			[0] NumericString [[s nsap]] ,

                applic-info
			[1] SEQUENCE OF [[T struct str_seq * $ applic_info ]] <<ss_next>>
                        	VisibleString [[s ss_str]]
                } ,

        osi-nsap-applic-relays
		[11] SEQUENCE [[T struct addr_info * $ *]]
                {
                nsap
			[0] NumericString [[s nsap]] ,

                applic-relay
			[1] SEQUENCE OF [[T struct str_seq * $ applic_info ]] <<ss_next>>
                                VisibleString [[s ss_str]]
                } ,

	dte-ybts-osi-addressing
		[12] SEQUENCE [[T struct addr_info * $ *]]
		{
                dte-number
			[0] NumericString [[s dte_number]] ,

                ybts-string
			[1] VisibleString [[s ybts_string]] ,

                tselector
			[2] OCTET STRING [[s tselector]]
                            OPTIONAL,

                sselector
			[3] OCTET STRING [[s sselector]]
                            OPTIONAL,

                pselector
			[4] OCTET STRING [[s pselector]]
                            OPTIONAL,

                place-holder
			[5] ANY [[a place_holder]]
                            OPTIONAL,

                application-title
			[6] ANY [[a application_title]]
                            OPTIONAL,

                per-application-context-info
			[7] ANY [[a per_app_context_info]]
                            OPTIONAL
		}
        }

NRSInformation [[P struct nrs_info *]]
::=
	SET
	{
		[0] Context [[p *]] ,
		[1] Address-space-id [[p *]] ,
	routes
		[2] SEQUENCE OF [[T struct nrs_routes * $ routes]] <<next>>
			SEQUENCE [[T struct nrs_routes * $ *]]
			{
				Route-cost [[p cost]] ,
				Addressing-info [[p addr_info]]
			}
	}

END
