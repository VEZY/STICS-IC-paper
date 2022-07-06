################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../src/inputs/USM/USM.f90 

OBJS += \
./src/inputs/USM/USM.o 


# Each subdirectory must supply rules for building sources it contributes
src/inputs/USM/%.o: ../src/inputs/USM/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/inputs/USM/USM.o: ../src/inputs/USM/USM.f90 src/outputs/history/Messages.o


