# Ensure the $(MODDIR) exists

SUFFIXES = .$(FC_MODEXT) _mod.$(FC_MODEXT)
.F90.$(FC_MODEXT) .F90_mod.$(FC_MODEXT) .f90.$(FC_MODEXT) .f90_mod.$(FC_MODEXT):
	test -d $(MODDIR) || mkdir -p $(MODDIR)
	$(PPFCCOMPILE) -c $<
	test -e $(MODDIR)/$@ && @cp $(MODDIR)/$@ .

CLEANFILES = *.$(FC_MODEXT) $(BUILT_SOURCES:%=$(MODDIR)/%) *__genmod.$(FC_MODEXT) *__genmod.f90
