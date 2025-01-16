
#include <numeric_ovf>

#include <array>
#include <cstdint>
#include <limits>
#include <type_traits>
#include <format>
#include <tuple>
#include <bit>

#include <gtest/gtest.h>

template<typename T>
class numeric_ovf_typed_test_T : public testing::Test {
protected:
	numeric_ovf_typed_test_T() {}
};


using supported_types = ::testing::Types<
	uint8_t,
	uint16_t,
	uint32_t,
	uint64_t,
	int8_t,
	int16_t,
	int32_t,
	int64_t
>;

namespace __OVERFLOW_TEST_PRIVATE__
{
	template<uintptr_t TSIZE, bool TSIGNED>
	struct int_clobber_type;

	template<> struct int_clobber_type<sizeof(uint8_t ), false> { using type = uint8_t ; };
	template<> struct int_clobber_type<sizeof(uint16_t), false> { using type = uint16_t; };
	template<> struct int_clobber_type<sizeof(uint32_t), false> { using type = uint32_t; };
	template<> struct int_clobber_type<sizeof(uint64_t), false> { using type = uint64_t; };
	template<> struct int_clobber_type<sizeof(int8_t ), true> { using type = int8_t ; };
	template<> struct int_clobber_type<sizeof(int16_t), true> { using type = int16_t; };
	template<> struct int_clobber_type<sizeof(int32_t), true> { using type = int32_t; };
	template<> struct int_clobber_type<sizeof(int64_t), true> { using type = int64_t; };

	template<typename T>
	using int_clobber_t = int_clobber_type<sizeof(T), std::is_signed_v<T>>::type;

	template<typename T>
	using uint_clobber_t = int_clobber_type<sizeof(T), false>::type;

} //namespace __OVERFLOW__PRIVATE__




template <typename TCase, uintptr_t Size>
consteval std::tuple<bool, uintptr_t> constexpr_check(std::array<TCase, Size> const& testCases)
{
	uintptr_t index = 0;

	for(TCase const& tcase: testCases)
	{
		if(!tcase.constexpr_check())
		{
			return {false, index};
		}

		++index;
	}

	return {true, 0};
}

template <typename TCase, uintptr_t Size>
consteval uintptr_t constexpr_fail_index(std::array<TCase, Size> const& testCases)
{
	return std::get<1>(constexpr_check(testCases));
}

template <typename TCase, uintptr_t Size>
consteval bool constexpr_test(std::array<TCase, Size> const& testCases)
{
	return std::get<0>(constexpr_check(testCases));
}



TYPED_TEST_SUITE(numeric_ovf_typed_test_T, supported_types);

TYPED_TEST(numeric_ovf_typed_test_T, add_carry)
{
	using num_T = TypeParam;

	struct TestCase_t
	{
		num_T const vA;
		num_T const vB;
		bool const carry;
		bool const overflow;
		std::string format() const
		{
			return std::format("vA={} vB={} c={}", vA, vB, carry);
		}

		consteval bool constexpr_check() const
		{
			using uint_clober_t = __OVERFLOW_TEST_PRIVATE__::uint_clobber_t<num_T>;

			num_T const expected = static_cast<num_T>(std::bit_cast<uint_clober_t>(vA) + std::bit_cast<uint_clober_t>(vB) + static_cast<uint_clober_t>(carry));
			{
				std::add_carry_result const result = std::add_carry(vA, vB, carry);
				if(result.low_bits != expected || result.overflow != overflow)
				{
					return false;
				}
			}
			if(vA != vB)
			{
				std::add_carry_result const result = std::add_carry(vB, vA, carry);
				if(result.low_bits != expected || result.overflow != overflow)
				{
					return false;
				}
			}
			return true;
		}

	};

	constexpr num_T max = std::numeric_limits<num_T>::max();
	constexpr num_T half_max = max / 2;

	{
		std::array constexpr testCases
		{
			TestCase_t{.vA =   0, .vB =   0, .carry = false, .overflow = false},
			TestCase_t{.vA =   0, .vB =   0, .carry = true , .overflow = false},
			TestCase_t{.vA =   1, .vB =   0, .carry = false, .overflow = false},
			TestCase_t{.vA =   1, .vB =   0, .carry = true , .overflow = false},
			TestCase_t{.vA =   1, .vB =   1, .carry = false, .overflow = false},
			TestCase_t{.vA =   1, .vB =   1, .carry = true , .overflow = false},
			TestCase_t{.vA = max, .vB =   0, .carry = false, .overflow = false},
			TestCase_t{.vA = max, .vB =   0, .carry = true , .overflow = true },
			TestCase_t{.vA = max, .vB =   1, .carry = false, .overflow = true },
			TestCase_t{.vA = max - 63, .vB = 62, .carry = false, .overflow = false},
			TestCase_t{.vA = max, .vB = max, .carry = false, .overflow = true },
			TestCase_t{.vA = max, .vB = max, .carry = true , .overflow = true },
			TestCase_t{.vA = half_max, .vB = half_max, .carry = true , .overflow = false},
			TestCase_t{.vA = half_max, .vB = half_max + 1, .carry = false, .overflow = false},
			TestCase_t{.vA = half_max, .vB = half_max + 1, .carry = true , .overflow = true },
			TestCase_t{.vA = half_max, .vB = half_max + 2, .carry = false, .overflow = true },
			TestCase_t{.vA = half_max - 36, .vB = half_max + 38, .carry = false, .overflow = true },
			TestCase_t{.vA = half_max - 54, .vB = half_max + 55, .carry = false, .overflow = false},
			TestCase_t{.vA = half_max - 54, .vB = half_max + 55, .carry = true , .overflow = true },
			TestCase_t{.vA = half_max - 23, .vB = half_max + 55, .carry = true , .overflow = true },
			TestCase_t{.vA = half_max - 23, .vB = half_max + 55, .carry = false, .overflow = true },
			TestCase_t{.vA = half_max - 54, .vB = half_max + 32, .carry = false, .overflow = false},
			TestCase_t{.vA = half_max - 54, .vB = half_max + 32, .carry = true , .overflow = false},
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();


		for(TestCase_t const& testCase: testCases)
		{
			num_T const expected = static_cast<num_T>(testCase.vA + testCase.vB + static_cast<num_T>(testCase.carry));
			{
				std::add_carry_result const result = std::add_carry(testCase.vA, testCase.vB, testCase.carry);
				ASSERT_EQ(result.low_bits, expected) << testCase.format();
				ASSERT_EQ(result.overflow, testCase.overflow) << testCase.format();
			}
			if(testCase.vA != testCase.vB)
			{
				std::add_carry_result const result = std::add_carry(testCase.vB, testCase.vA, testCase.carry);
				ASSERT_EQ(result.low_bits, expected) << "reversed " << testCase.format();
				ASSERT_EQ(result.overflow, testCase.overflow) << "reversed " << testCase.format();
			}
		}
	}


	if constexpr (std::is_signed_v<num_T>)
	{
		constexpr num_T min = std::numeric_limits<num_T>::min();
		constexpr num_T half_min = min / 2;

		std::array constexpr testCases
		{
			TestCase_t{.vA =   0, .vB = -1, .carry = false, .overflow = false},
			TestCase_t{.vA =   0, .vB = -1, .carry = true , .overflow = false},
			TestCase_t{.vA = min, .vB =  0, .carry = false, .overflow = false},
			TestCase_t{.vA = min, .vB =  0, .carry = true , .overflow = false},
			TestCase_t{.vA = min, .vB =  1, .carry = false, .overflow = false},
			TestCase_t{.vA = min, .vB =  1, .carry = true , .overflow = false},
			TestCase_t{.vA = min, .vB = -1, .carry = false, .overflow = true},
			TestCase_t{.vA = min, .vB = -1, .carry = true , .overflow = false},
			TestCase_t{.vA = max, .vB = -1, .carry = false, .overflow = false},
			TestCase_t{.vA = max, .vB = -1, .carry = true , .overflow = false},
			TestCase_t{.vA = max, .vB = min, .carry = false, .overflow = false},
			TestCase_t{.vA = max, .vB = min, .carry = true , .overflow = false},
			TestCase_t{.vA = half_min, .vB = half_min, .carry = false, .overflow = false},
			TestCase_t{.vA = half_min, .vB = half_min, .carry = true , .overflow = false},
			TestCase_t{.vA = half_min, .vB = half_min - 1, .carry = false, .overflow = true},
			TestCase_t{.vA = half_min, .vB = half_min - 1, .carry = true, .overflow = false},
			TestCase_t{.vA = half_min, .vB = half_min - 2, .carry = true, .overflow = true},
			TestCase_t{.vA = half_min + 53, .vB = half_min - 55, .carry = false, .overflow = true},
			TestCase_t{.vA = half_min + 10, .vB = half_min - 60, .carry = false, .overflow = true},
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for(TestCase_t const& testCase: testCases)
		{
			num_T const expected = static_cast<num_T>(testCase.vA + testCase.vB + static_cast<num_T>(testCase.carry));
			{
				std::add_carry_result const result = std::add_carry(testCase.vA, testCase.vB, testCase.carry);
				ASSERT_EQ(result.low_bits, expected) << testCase.format();
				ASSERT_EQ(result.overflow, testCase.overflow) << testCase.format();
			}
			if(testCase.vA != testCase.vB)
			{
				std::add_carry_result const result = std::add_carry(testCase.vB, testCase.vA, testCase.carry);
				ASSERT_EQ(result.low_bits, expected) << "reversed " << testCase.format();
				ASSERT_EQ(result.overflow, testCase.overflow) << "reversed " << testCase.format();
			}
		}
	}

}

TYPED_TEST(numeric_ovf_typed_test_T, sub_borrow)
{
	using num_T = TypeParam;

	struct TestCase_t
	{
		num_T const vA;
		num_T const vB;
		bool const borrow;
		bool const overflow;
		std::string format() const
		{
			return std::format("vA={} vB={} b={}", vA, vB, borrow);
		}

		consteval bool constexpr_check() const
		{
			using uint_clober_t = __OVERFLOW_TEST_PRIVATE__::uint_clobber_t<num_T>;

			num_T const expected = static_cast<num_T>(std::bit_cast<uint_clober_t>(vA) - std::bit_cast<uint_clober_t>(vB) - static_cast<uint_clober_t>(borrow));
			std::add_carry_result const result = std::sub_borrow(vA, vB, borrow);
			return (result.low_bits == expected) && (result.overflow == overflow);
		}
	};

	constexpr num_T max = std::numeric_limits<num_T>::max();
	constexpr num_T half_max = max / 2;

	if constexpr (std::is_unsigned_v<num_T>)
	{
		std::array constexpr testCases
		{
			TestCase_t{.vA =   0, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA =   0, .vB =   0, .borrow = true , .overflow = true},
			TestCase_t{.vA =   1, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA =   1, .vB =   0, .borrow = true , .overflow = false},
			TestCase_t{.vA =   0, .vB =   1, .borrow = false, .overflow = true},
			TestCase_t{.vA =   0, .vB =   1, .borrow = true , .overflow = true},
			TestCase_t{.vA =   1, .vB =   1, .borrow = false, .overflow = false},
			TestCase_t{.vA =   1, .vB =   1, .borrow = true , .overflow = true},
			TestCase_t{.vA =   2, .vB =   1, .borrow = false, .overflow = false},
			TestCase_t{.vA =   2, .vB =   1, .borrow = true , .overflow = false},
			TestCase_t{.vA =   33, .vB = 54, .borrow = false, .overflow = true},
			TestCase_t{.vA =   34, .vB = 45, .borrow = true , .overflow = true},
			TestCase_t{.vA =   0, .vB = max, .borrow = false, .overflow = true},
			TestCase_t{.vA =   0, .vB = max, .borrow = true , .overflow = true},
			TestCase_t{.vA = max, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA = max, .vB =   0, .borrow = true , .overflow = false},
			TestCase_t{.vA = max, .vB =  37, .borrow = false, .overflow = false},
			TestCase_t{.vA = max, .vB =  46, .borrow = true , .overflow = false},
			TestCase_t{.vA = max, .vB =  half_max, .borrow = false, .overflow = false},
			TestCase_t{.vA = max, .vB =  half_max, .borrow = true , .overflow = false},
			TestCase_t{.vA = half_max, .vB =  max, .borrow = false, .overflow = true},
			TestCase_t{.vA = half_max, .vB =  max, .borrow = true , .overflow = true},
			TestCase_t{.vA = max, .vB =  max, .borrow = false, .overflow = false},
			TestCase_t{.vA = max, .vB =  max, .borrow = true , .overflow = true},
			TestCase_t{.vA = max, .vB =  max - 1, .borrow = true , .overflow = false},
			TestCase_t{.vA = half_max, .vB =  half_max, .borrow = false, .overflow = false},
			TestCase_t{.vA = half_max, .vB =  half_max, .borrow = true , .overflow = true},
			TestCase_t{.vA = half_max, .vB =  half_max - 1, .borrow = true , .overflow = false},
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for(TestCase_t const& testCase: testCases)
		{
			num_T const expected = static_cast<num_T>(testCase.vA - testCase.vB - static_cast<num_T>(testCase.borrow));
			{
				std::add_carry_result const result = std::sub_borrow(testCase.vA, testCase.vB, testCase.borrow);
				ASSERT_EQ(result.low_bits, expected) << testCase.format();
				ASSERT_EQ(result.overflow, testCase.overflow) << testCase.format();
			}
		}

	}
	else if constexpr (std::is_signed_v<num_T>)
	{
		constexpr num_T min = std::numeric_limits<num_T>::min();
		constexpr num_T half_min = min / 2;

		std::array constexpr testCases
		{
			TestCase_t{.vA =   0, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA =   0, .vB =   0, .borrow = true , .overflow = false},
			TestCase_t{.vA =   1, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA =   1, .vB =   0, .borrow = true , .overflow = false},
			TestCase_t{.vA =   0, .vB =   1, .borrow = false, .overflow = false},
			TestCase_t{.vA =   0, .vB =   1, .borrow = true , .overflow = false},
			TestCase_t{.vA =   1, .vB =   1, .borrow = false, .overflow = false},
			TestCase_t{.vA =   1, .vB =   1, .borrow = true , .overflow = false},
			TestCase_t{.vA =   2, .vB =   1, .borrow = false, .overflow = false},
			TestCase_t{.vA =   2, .vB =   1, .borrow = true , .overflow = false},
			TestCase_t{.vA =  -1, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA =  -1, .vB =   0, .borrow = true , .overflow = false},
			TestCase_t{.vA =   0, .vB =  -1, .borrow = false, .overflow = false},
			TestCase_t{.vA =   0, .vB =  -1, .borrow = true , .overflow = false},
			TestCase_t{.vA =  -1, .vB =  -1, .borrow = false, .overflow = false},
			TestCase_t{.vA =  -1, .vB =  -1, .borrow = true , .overflow = false},
			TestCase_t{.vA =  -2, .vB =  -1, .borrow = false, .overflow = false},
			TestCase_t{.vA =  -2, .vB =  -1, .borrow = true , .overflow = false},
			TestCase_t{.vA =   33, .vB = 54, .borrow = false, .overflow = false},
			TestCase_t{.vA =   0, .vB = max, .borrow = false, .overflow = false},
			TestCase_t{.vA =   0, .vB = max, .borrow = true , .overflow = false},
			TestCase_t{.vA = max, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA = max, .vB =   0, .borrow = true , .overflow = false},
			TestCase_t{.vA = max, .vB =  37, .borrow = false, .overflow = false},
			TestCase_t{.vA = max, .vB =  46, .borrow = true , .overflow = false},
			TestCase_t{.vA = max, .vB =  half_max, .borrow = false, .overflow = false},
			TestCase_t{.vA = max, .vB =  half_max, .borrow = true , .overflow = false},
			TestCase_t{.vA = half_max, .vB =  max, .borrow = false, .overflow = false},
			TestCase_t{.vA = half_max, .vB =  max, .borrow = true , .overflow = false},
			TestCase_t{.vA =   0, .vB = min, .borrow = false, .overflow = true},
			TestCase_t{.vA =   0, .vB = min, .borrow = true , .overflow = false},
			TestCase_t{.vA = min, .vB =   0, .borrow = false, .overflow = false},
			TestCase_t{.vA = min, .vB =   0, .borrow = true , .overflow = true},
			TestCase_t{.vA = half_max, .vB =  half_min, .borrow = false, .overflow = false},
			TestCase_t{.vA = half_max, .vB =  half_min, .borrow = true , .overflow = false},
			TestCase_t{.vA = half_max + 1, .vB = half_min, .borrow = false, .overflow = true},
			TestCase_t{.vA = half_max + 1, .vB = half_min, .borrow = true , .overflow = false},
			TestCase_t{.vA = half_max, .vB =  half_min - 1, .borrow = false, .overflow = true},
			TestCase_t{.vA = half_max, .vB =  half_min - 1, .borrow = true , .overflow = false},
			TestCase_t{.vA = half_min, .vB =  half_max, .borrow = false, .overflow = false},
			TestCase_t{.vA = half_min, .vB =  half_max, .borrow = true , .overflow = false},
			TestCase_t{.vA = half_min, .vB =  half_max + 1, .borrow = false, .overflow = false},
			TestCase_t{.vA = half_min, .vB =  half_max + 1, .borrow = true , .overflow = true},
			TestCase_t{.vA = half_min, .vB =  half_max + 2, .borrow = false, .overflow = true},
			TestCase_t{.vA = half_min, .vB =  half_max + 2, .borrow = true , .overflow = true},
			TestCase_t{.vA = min, .vB = max, .borrow = false, .overflow = true},
			TestCase_t{.vA = min, .vB = max, .borrow = true , .overflow = true},
		};


		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for(TestCase_t const& testCase: testCases)
		{
			num_T const expected = static_cast<num_T>(testCase.vA - testCase.vB - static_cast<num_T>(testCase.borrow));
			{
				std::add_carry_result const result = std::sub_borrow(testCase.vA, testCase.vB, testCase.borrow);
				ASSERT_EQ(result.low_bits, expected) << testCase.format();
				ASSERT_EQ(result.overflow, testCase.overflow) << testCase.format();
			}
		}
	}
}


TYPED_TEST(numeric_ovf_typed_test_T, mul_wide)
{
	using num_T = TypeParam;

	struct TestCase_t
	{
		num_T const vA;
		num_T const vB;
		num_T const e_low;
		num_T const e_hi;

		std::string format() const
		{
			return std::format("vA={} vB={}", vA, vB);
		}

		consteval bool constexpr_check() const
		{
			{
				std::mul_wide_result const result = std::mul_wide(vA, vB);
				if(result.low_bits != e_low || result.high_bits != e_hi)
				{
					return false;
				}
			}
			if(vA != vB)
			{
				std::mul_wide_result const result = std::mul_wide(vB, vA);
				if(result.low_bits != e_low || result.high_bits != e_hi)
				{
					return false;
				}
			}
			return true;
		}
	};

	constexpr num_T max = std::numeric_limits<num_T>::max();

	if constexpr (std::is_unsigned_v<num_T>)
	{
		std::array constexpr testCases
		{
			TestCase_t{.vA =   0, .vB =   0, .e_low =   0, .e_hi = 0},
			TestCase_t{.vA =   0, .vB =  57, .e_low =   0, .e_hi = 0},
			TestCase_t{.vA = max, .vB = max, .e_low =   1, .e_hi = max - 1 },
			TestCase_t{.vA =  57, .vB =   4, .e_low = 228, .e_hi = 0 },
			TestCase_t{.vA = 137, .vB =   1, .e_low = 137, .e_hi = 0 },
			TestCase_t{.vA = max, .vB = 231, .e_low = static_cast<num_T>(-231), .e_hi = 231 - 1 },
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for(TestCase_t const& testCase: testCases)
		{
			{
				std::mul_wide_result const result = std::mul_wide(testCase.vA, testCase.vB);
				ASSERT_EQ(result.low_bits, testCase.e_low) << testCase.format();
				ASSERT_EQ(result.high_bits, testCase.e_hi) << testCase.format();
			}
			if(testCase.vA != testCase.vB)
			{
				std::mul_wide_result const result = std::mul_wide(testCase.vB, testCase.vA);
				ASSERT_EQ(result.low_bits, testCase.e_low) << testCase.format();
				ASSERT_EQ(result.high_bits, testCase.e_hi) << testCase.format();
			}
		}
	}
	else if constexpr (std::is_signed_v<num_T>)
	{
		constexpr num_T min = std::numeric_limits<num_T>::min();
		constexpr num_T max_res = static_cast<num_T>(num_T{1} << (sizeof(num_T) * 8 - 2));
		constexpr num_T max_min = static_cast<num_T>(num_T{3} << (sizeof(num_T) * 8 - 2));

		std::array constexpr testCases
		{
			TestCase_t{.vA =   0, .vB =   0, .e_low =   0, .e_hi = 0},
			TestCase_t{.vA =   0, .vB =  57, .e_low =   0, .e_hi = 0},
			TestCase_t{.vA = max, .vB = max, .e_low =   1, .e_hi = static_cast<num_T>(max >> 1) },
			TestCase_t{.vA =  37, .vB =   3, .e_low = 111, .e_hi = 0},
			TestCase_t{.vA = min, .vB = min, .e_low =   0, .e_hi = max_res},
			TestCase_t{.vA =   1, .vB =  -1, .e_low =  -1, .e_hi = -1},
			TestCase_t{.vA =  -1, .vB =  -1, .e_low =   1, .e_hi = 0},
			TestCase_t{.vA =  23, .vB =  -5, .e_low = -115, .e_hi = -1},
			TestCase_t{.vA = max, .vB = min, .e_low = min, .e_hi = max_min},
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for(TestCase_t const& testCase: testCases)
		{
			{
				std::mul_wide_result const result = std::mul_wide(testCase.vA, testCase.vB);
				ASSERT_EQ(result.low_bits, testCase.e_low) << testCase.format();
				ASSERT_EQ(result.high_bits, testCase.e_hi) << testCase.format();
			}
			if(testCase.vA != testCase.vB)
			{
				std::mul_wide_result const result = std::mul_wide(testCase.vB, testCase.vA);
				ASSERT_EQ(result.low_bits, testCase.e_low) << testCase.format();
				ASSERT_EQ(result.high_bits, testCase.e_hi) << testCase.format();
			}
		}
	}

}

TYPED_TEST(numeric_ovf_typed_test_T, div_wide)
{
	using num_T = TypeParam;

	struct TestCase_t
	{
		num_T const dividend_low;
		num_T const dividend_high;
		num_T const divisor;
		num_T const quotient;
		num_T const remainder;

		std::string format() const
		{
			return std::format("div_l={} div_h={} d={}", dividend_low, dividend_high, divisor);
		}

		consteval bool constexpr_check() const
		{
			std::div_result const result = std::div_wide(dividend_high, dividend_low, divisor);
			return (result.quotient == quotient) && (result.remainder == remainder);
		}
	};

	constexpr num_T max = std::numeric_limits<num_T>::max();

	if constexpr (std::is_unsigned_v<num_T>)
	{
		std::array constexpr testCases
		{
			TestCase_t{.dividend_low = max - 1, .dividend_high = max - 1, .divisor = max, .quotient = max, .remainder = max - 2},
			TestCase_t{.dividend_low = max, .dividend_high = max - 1, .divisor = max, .quotient = max, .remainder = max - 1},
			TestCase_t{.dividend_low = max, .dividend_high = 1, .divisor = max, .quotient = 2, .remainder = 1},
			TestCase_t{.dividend_low = 0, .dividend_high = 1, .divisor = max, .quotient = 1, .remainder = 1},
			TestCase_t{.dividend_low = 0, .dividend_high = 0, .divisor = 37, .quotient = 0, .remainder = 0},
			TestCase_t{.dividend_low = 98, .dividend_high = 0, .divisor = 175, .quotient = 0, .remainder = 98},
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for(TestCase_t const& testCase: testCases)
		{
			std::div_result const result = std::div_wide(testCase.dividend_high, testCase.dividend_low, testCase.divisor);
			ASSERT_EQ(result.quotient, testCase.quotient) << testCase.format();
			ASSERT_EQ(result.remainder, testCase.remainder) << testCase.format();
		}

	}
	else if constexpr (std::is_signed_v<num_T>)
	{
		constexpr num_T min = std::numeric_limits<num_T>::min();
		constexpr num_T max_res = static_cast<num_T>(num_T{1} << (sizeof(num_T) * 8 - 2));
		constexpr num_T max_min = static_cast<num_T>(num_T{3} << (sizeof(num_T) * 8 - 2));

		std::array constexpr testCases
		{
			TestCase_t{.dividend_low = max - 1, .dividend_high = static_cast<num_T>(max >> 1), .divisor = max, .quotient = max, .remainder = max - 2},
			TestCase_t{.dividend_low = max, .dividend_high = static_cast<num_T>(max >> 1), .divisor = max, .quotient = max, .remainder = max - 1},
			TestCase_t{.dividend_low =   0, .dividend_high = 0, .divisor = 37, .quotient = 0, .remainder = 0},
			TestCase_t{.dividend_low =  98, .dividend_high = 0, .divisor = 111, .quotient = 0, .remainder = 98},
			TestCase_t{.dividend_low = max, .dividend_high = max_res, .divisor = min, .quotient = min, .remainder = max},
			TestCase_t{.dividend_low = min, .dividend_high = max_min, .divisor = min, .quotient = max, .remainder = 0},
			TestCase_t{.dividend_low = min, .dividend_high = max_min, .divisor = max, .quotient = min, .remainder = 0},
			TestCase_t{.dividend_low =   1, .dividend_high = max_min, .divisor = min, .quotient = max, .remainder = -max},
			TestCase_t{.dividend_low =   2, .dividend_high = max_min, .divisor = max, .quotient = min, .remainder = -max + 1},
		};

		constexpr bool constexpr_result = constexpr_test(testCases);
		constexpr uintptr_t constexpr_findex = constexpr_fail_index(testCases);

		ASSERT_TRUE(constexpr_result) << "constexpr " << testCases[constexpr_findex].format();

		for(TestCase_t const& testCase: testCases)
		{
			std::div_result const result = std::div_wide(testCase.dividend_high, testCase.dividend_low, testCase.divisor);
			ASSERT_EQ(result.quotient, testCase.quotient) << testCase.format();
			ASSERT_EQ(result.remainder, testCase.remainder) << testCase.format();
		}
	}
}
