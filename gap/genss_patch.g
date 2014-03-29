###################################################
#
# In the GenSS package, there is a mistake in the 
# operation Stab: every occurrence of SetParent()
# has the arguments the wrong way around! So here 
# is a patch.
#
###################################################



InstallMethod( Stab, "by Orb orbit enumeration",
  [IsGroup, IsObject, IsFunction, IsRecord],
  function( g, x, op, opt )
    local S,count,el,errorprob,found,gens,i,j,limit,memperpt,nrrand,o,
          pat,pos,pr,res,stab,stabchain,stabel,stabgens,stabsizeest,w1,w2,y;
    GENSS_CopyDefaultOptions(GENSS,opt);
    if HasStoredStabilizerChain(g) then
      S := StoredStabilizerChain(g);
      if IsIdenticalObj(S!.orb!.op,op) and x in S!.orb then
        if S!.stab = false then
            y := rec( stab := TrivialSubgroup(g), size := 1, proof := true );
            y.stabilizerchain := StabilizerChain(y.stab);
            return y;
        fi;
        pos := Position(S!.orb,x);
        w1 := TraceSchreierTreeForward(S!.orb,pos);
        y := GENSS_Prod(S!.orb!.gens,w1);
        stab := Group(List(S!.stab!.orb!.gens,a->y^-1*a*y));
        return rec( stab := stab, size := Size(S!.stab),
                    stabilizerchain := StabilizerChain(stab,rec( Base := S)),
                    proof := true );
      fi;
    fi;
    # Now we have to do it ourselves:
    if not(IsBound(opt.ErrorBound)) then   # we are working deterministically
        if not(HasSize(g)) then
            # We are basically stuffed, unless we want to check all
            # Schreier generators of an orbit!
            Error("I do not want to check all Schreier generators, ",
                  "need size of group or ErrorBound option!");
            return fail;
        fi;
    else
        if opt.ErrorBound < opt.StabAssumeCompleteLimit then
            opt.StabAssumeCompleteLimit := opt.ErrorBound;
        fi;
    fi;
    # Now we either know the group size or work Monte Carlo:
    errorprob := 1;
    limit := opt.StabInitialLimit;
    pat := opt.StabInitialPatience;
    o := Orb(g,x,op,rec( report := opt.Report, 
                         treehashsize := opt.InitialHashSize,
                         schreier := true ) );
    stabgens := [];
    stabsizeest := 1;
    pr := ProductReplacer(GeneratorsOfGroup(g),
                rec( scramble := opt.StabScramble, 
                     scramblefactor := opt.StabScrambleFactor,
                     addslots := opt.StabAddSlots,
                     maxdepth := opt.StabMaxDepth ));
    gens := GeneratorsOfGroup(g);
    # Now go through a cycle of orbit enumeration and stabilizer generation:
    repeat
        if not(IsClosed(o)) then
            if HasSize(g) and limit > QuoInt(Size(g),stabsizeest*2)+1 then
                limit := QuoInt(Size(g),stabsizeest*2)+2;
            fi;
            Info(InfoGenSS,2,"Enumerating orbit with limit ",limit);
            Enumerate(o,limit);
            if IsClosed(o) then
                Info(InfoGenSS,2,"Orbit closed, size is ",Length(o));
            fi;
        fi;
        if errorprob < opt.StabAssumeCompleteLimit then 
            if HasSize(g) and 
               2 * Length(o) * stabsizeest > Size(g) then
                # Done!
                #stab := Subgroup(g,stabgens);
                if Length(stabgens) > 0 then
                    stab := Group(stabgens);
                    SetParent(stab, g);
                    SetSize(stab,stabsizeest);
                    SetStabilizerChain(stab,stabchain);
                else
                    stab := TrivialSubgroup(g);
                    stabchain := StabilizerChain(stab);
                fi;
                return rec( stab := stab, size := stabsizeest, 
                            stabilizerchain := stabchain,
                            proof := true );
            fi;
            limit := 2*limit;
            continue; 
        fi;
        count := 0;
        found := false;
        repeat
            el := Next(pr);
            i := 1;
            while i <= Length(o) and not(found) do
                y := op(o[i],el);
                j := Position(o,y);
                if j <> fail then 
                    found := true; 
                else
                    i := i + 1;
                fi;
            od;
            count := count + 1;
        until found or count >= pat;
        if not(IsClosed(o)) then
            if not(found) then
                limit := QuoInt(3*limit,2);
            else
                if count < 10 then
                    limit := limit + 100;
                else
                    limit := QuoInt(3*limit,2);
                fi;
            fi;
        fi;
        pat := QuoInt(pat*5,4);
        if found then   # Have a potential stabiliser element:
            Info(InfoGenSS,3,"Found a potential stabilising element...");
            w1 := TraceSchreierTreeForward(o,i);
            w2 := TraceSchreierTreeForward(o,j);
            stabel := EvaluateWord(gens,w1)*el/EvaluateWord(gens,w2);
            if IsOne(stabel) then
                    errorprob := errorprob / 2;
                    Info(InfoGenSS,3,"... which was the identity.");
            else
                Add(stabgens,stabel);
                if Length(stabgens) < 2 then
                    Info(InfoGenSS,3,"Waiting for second stabiliser element.");
                else      # Length(stabgens) >= 2 then
                    if Length(stabgens) = 2 then
                        Info(InfoGenSS,2,"Estimating stabilizer...");
                        stabchain := StabilizerChain(Group(stabgens));
                        errorprob := 1;
                    else
                        res := AddGeneratorToStabilizerChain(stabchain,stabel);
                        if not(res) then
                            Remove(stabgens,Length(stabgens));
                            errorprob := errorprob / 2;
                            Info(InfoGenSS,2,"Error probablity now < ",
                                 errorprob);
                        else
                            Info(InfoGenSS,2,
                                 "Added generator to stabilizer...");
                            VerifyStabilizerChainMC(stabchain,10);
                            errorprob := 1;
                        fi;
                    fi;
                    if Size(stabchain) > stabsizeest then
                        stabsizeest := Size(stabchain);
                        Info(InfoGenSS,2,"New stabilizer estimate: ",
                             stabsizeest);
                    else
                        Info(InfoGenSS,2,"Stabiliser estimate unchanged.");
                    fi;
                    if HasSize(g) and
                       2 * Length(o) * stabsizeest > Size(g) then
                        # Done!
                        if Length(stabgens) > 0 then
                            stab := Group(stabgens);
                            SetParent(stab,g);
                            SetSize(stab,stabsizeest);
                            SetStabilizerChain(stab,stabchain);
                        else
                            stab := TrivialSubgroup(g);
                            stabchain := StabilizerChain(stab);
                        fi;
                        return rec( stab := stab, size := stabsizeest, 
                                    stabilizerchain := stabchain,
                                    proof := true );
                    fi;
                    if IsBound(opt.ErrorBound) and
                       errorprob < opt.ErrorBound then
                        #stab := Subgroup(g,stabgens);
                        if Length(stabgens) > 0 then
                            stab := Group(stabgens);
                            SetParent(stab, g);
                        else
                            stab := TrivialSubgroup(g);
                            stabchain := StabilizerChain(stab);
                        fi;
                        return rec( stab := stab, size := stabsizeest,
                                    stabilizerchain := stabchain,
                                    proof := false );
                    fi;
                fi;
            fi;
        else   # no element found
            Info(InfoGenSS,3,"No stabiliser element found!");
        fi;
    until Length(o) > opt.StabOrbitLimit;
    return fail;
  end );
