################################################################################
# Automatically-generated file. Do not edit!
################################################################################

-include ../makefile.init

RM := rm -rf

# All of the sources participating in the build are defined here
-include sources.mk
-include src/utilities/soil/subdir.mk
-include src/utilities/dates_management/subdir.mk
-include src/utilities/climate/subdir.mk
-include src/utilities/subdir.mk
-include src/stics_day/stress/subdir.mk
-include src/stics_day/offer_requirement_N/subdir.mk
-include src/stics_day/offer_requirement_H2o/subdir.mk
-include src/stics_day/micro_climate/subdir.mk
-include src/stics_day/growth/subdir.mk
-include src/stics_day/decision_support/subdir.mk
-include src/stics_day/cut_crops/subdir.mk
-include src/stics_day/contibutions_N_H2O_residues/subdir.mk
-include src/stics_day/Lixivation/subdir.mk
-include src/stics_day/Development/subdir.mk
-include src/stics_day/subdir.mk
-include src/outputs/report/subdir.mk
-include src/outputs/profile/subdir.mk
-include src/outputs/history/subdir.mk
-include src/outputs/data_end_ofCycle/subdir.mk
-include src/outputs/daily_outputs/subdir.mk
-include src/outputs/Balance/subdir.mk
-include src/inputs/outputs_management/subdir.mk
-include src/inputs/optimisation/subdir.mk
-include src/inputs/initializations/subdir.mk
-include src/inputs/generals_parameters/subdir.mk
-include src/inputs/data_previousCycle/subdir.mk
-include src/inputs/crop_management_parameters/subdir.mk
-include src/inputs/USM/subdir.mk
-include src/inputs/Soil/subdir.mk
-include src/inputs/Plant/subdir.mk
-include src/inputs/LAI/subdir.mk
-include src/inputs/Climate/subdir.mk
-include src/inputs/subdir.mk
-include src/initializations/subdir.mk
-include src/subdir.mk
-include subdir.mk
-include objects.mk

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(strip $(C_DEPS)),)
-include $(C_DEPS)
endif
endif

-include ../makefile.defs

# detecting OS type for setting -static flag
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	CFLAGS += -static
endif

ifeq ($(OS),Windows_NT)
	CFLAGS += -static
endif

# Add inputs and outputs from these tool invocations to the build variables

# All Target
all: Stics

# Tool invocations
Stics: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: GNU Fortran Linker'
	gfortran -fallow-argument-mismatch -g $(CFLAGS) -o "Stics" $(OBJS) $(USER_OBJS) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '


# Other Targets
clean:
	-$(RM) $(OBJS)$(C_DEPS)$(EXECUTABLES) Stics *.mod
	-@echo ' '

clean2:
	find -name "*.o" -exec rm {} \;
	find -name "*.mod" -exec rm {} \;

.PHONY: all clean dependents
.SECONDARY:

-include ../makefile.targets
