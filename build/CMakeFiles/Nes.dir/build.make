# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.19

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Disable VCS-based implicit rules.
% : %,v


# Disable VCS-based implicit rules.
% : RCS/%


# Disable VCS-based implicit rules.
% : RCS/%,v


# Disable VCS-based implicit rules.
% : SCCS/s.%


# Disable VCS-based implicit rules.
% : s.%


.SUFFIXES: .hpux_make_needs_suffix_list


# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/Cellar/cmake/3.19.6/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/3.19.6/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/evandro/Documents/dev/nes

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/evandro/Documents/dev/nes/build

# Include any dependencies generated for this target.
include CMakeFiles/Nes.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Nes.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Nes.dir/flags.make

CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.o: CMakeFiles/Nes.dir/flags.make
CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.o: ../src/Components/Bus/Bus.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/evandro/Documents/dev/nes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.o"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.o -c /Users/evandro/Documents/dev/nes/src/Components/Bus/Bus.cpp

CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.i"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/evandro/Documents/dev/nes/src/Components/Bus/Bus.cpp > CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.i

CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.s"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/evandro/Documents/dev/nes/src/Components/Bus/Bus.cpp -o CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.s

CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.o: CMakeFiles/Nes.dir/flags.make
CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.o: ../src/Components/Processor/Processor.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/evandro/Documents/dev/nes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.o"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.o -c /Users/evandro/Documents/dev/nes/src/Components/Processor/Processor.cpp

CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.i"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/evandro/Documents/dev/nes/src/Components/Processor/Processor.cpp > CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.i

CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.s"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/evandro/Documents/dev/nes/src/Components/Processor/Processor.cpp -o CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.s

CMakeFiles/Nes.dir/src/main.cpp.o: CMakeFiles/Nes.dir/flags.make
CMakeFiles/Nes.dir/src/main.cpp.o: ../src/main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/evandro/Documents/dev/nes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object CMakeFiles/Nes.dir/src/main.cpp.o"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Nes.dir/src/main.cpp.o -c /Users/evandro/Documents/dev/nes/src/main.cpp

CMakeFiles/Nes.dir/src/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Nes.dir/src/main.cpp.i"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/evandro/Documents/dev/nes/src/main.cpp > CMakeFiles/Nes.dir/src/main.cpp.i

CMakeFiles/Nes.dir/src/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Nes.dir/src/main.cpp.s"
	/usr/bin/clang++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/evandro/Documents/dev/nes/src/main.cpp -o CMakeFiles/Nes.dir/src/main.cpp.s

# Object files for target Nes
Nes_OBJECTS = \
"CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.o" \
"CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.o" \
"CMakeFiles/Nes.dir/src/main.cpp.o"

# External object files for target Nes
Nes_EXTERNAL_OBJECTS =

Nes: CMakeFiles/Nes.dir/src/Components/Bus/Bus.cpp.o
Nes: CMakeFiles/Nes.dir/src/Components/Processor/Processor.cpp.o
Nes: CMakeFiles/Nes.dir/src/main.cpp.o
Nes: CMakeFiles/Nes.dir/build.make
Nes: CMakeFiles/Nes.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/evandro/Documents/dev/nes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Linking CXX executable Nes"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Nes.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Nes.dir/build: Nes

.PHONY : CMakeFiles/Nes.dir/build

CMakeFiles/Nes.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Nes.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Nes.dir/clean

CMakeFiles/Nes.dir/depend:
	cd /Users/evandro/Documents/dev/nes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/evandro/Documents/dev/nes /Users/evandro/Documents/dev/nes /Users/evandro/Documents/dev/nes/build /Users/evandro/Documents/dev/nes/build /Users/evandro/Documents/dev/nes/build/CMakeFiles/Nes.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Nes.dir/depend
