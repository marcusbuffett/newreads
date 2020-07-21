CREATE UNIQUE INDEX "recRequests_pkey" ON public."recRequests" USING btree ("Uuid") ;


CREATE INDEX ixrecrequests_created ON public."recRequests" USING btree ("Created") ;


CREATE UNIQUE INDEX "goodreadsBooks_pkey" ON public."goodreadsBooks" USING btree ("Uuid") ;


CREATE UNIQUE INDEX "recRequestProps_pkey" ON public."recRequestProps" USING btree ("Uuid") ;


CREATE UNIQUE INDEX "recRequestPropsJoins_PropUuid_RequestUuid_key" ON public."recRequestPropsJoins" USING btree ("PropUuid", "RequestUuid") ;


CREATE UNIQUE INDEX users_pkey ON public.users USING btree ("Uuid") ;


CREATE UNIQUE INDEX recommendations_pkey ON public.recommendations USING btree ("Uuid") ;


CREATE INDEX ixrecommendations_created ON public.recommendations USING btree ("Created") ;


CREATE UNIQUE INDEX "recommendationClickthroughs_pkey" ON public."recommendationClickthroughs" USING btree ("Uuid") ;
