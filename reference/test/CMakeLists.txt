cmake_minimum_required(VERSION 4.2.3)
project(tsd_ovf_test)
enable_language(CXX)

file(GLOB_RECURSE SOURCE_FILES CONFIGURE_DEPENDS 
	"./src/*.cpp"
)

add_executable(${PROJECT_NAME} ${SOURCE_FILES})

target_link_libraries(${PROJECT_NAME} LINK_PRIVATE std_ovf)
target_link_libraries(${PROJECT_NAME} LINK_PRIVATE gtest gtest_main)

quick_default_post_config()
