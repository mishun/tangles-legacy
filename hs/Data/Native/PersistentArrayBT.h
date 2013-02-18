#pragma once

#include <cassert>
#include <cstddef>
#include "./nullptr.h"

namespace Data { namespace Native {

	template<typename Type>
	class PersistentArrayBT
	{
	private:
		struct Node
		{
		private:
			size_t referenceCounter;
			enum { Array, Diff } type;

			union
			{
				Type * array;
				struct { Node * next; size_t key; Type value; } diff;
			};

		public:
			explicit Node(Type * a)
				: referenceCounter(1)
				, type(Array)
			{
				array = a;
			}

			void addRef()
			{
				referenceCounter++;
			}

		private:
			Node(Node * n, int k, const Type & v)
				: referenceCounter(1)
				, type(Diff)
			{
				diff.next = n;
				diff.key = k;
				diff.value = v;
			}

			~Node()
			{
				if(type == Array)
				{
					delete[] array;
					array = nullptr;
				}
				else
					diff.next = nullptr;
			}

		public:
			// TODO: implement
			static Node * reroot(Node * node)
			{
				if(node->type == Array)
					return node;

				Node * ret = reroot(node->diff.next);
				return ret;
			}

			static const Type & get(Node * node, size_t index)
			{
				while(true)
					if(node->type == Array)
						return node->array[index];
					else if(index == node->diff.key)
						return node->diff.value;
					else node = node->diff.next;
			}

			static Node * set(Node * node, int key, const Type & value)
			{
				if(node->type == Array)
				{
					auto old = node->array[key];
					node->array[key] = value;
					auto * cur = new Node(node->array);

					node->type = Diff;
					node->diff.key = key;
					node->diff.next = cur;
					node->diff.value = old;

					cur->referenceCounter++;
					return cur;
				}
				else
				{
					node->referenceCounter++;
					return new Node(node, key, value);
				}
			}

			static void release(Node * node)
			{
				while(node != 0)
				{
					node->referenceCounter--;
					if(node->referenceCounter > 0)
						return;

					auto * next = (node->type == Diff) ? node->diff.next : 0;
					delete node;

					node = next;
				}
			}
		};

	private:
		Node * node;

	private:
		constexpr PersistentArrayBT(Node * n)
			: node(n)
		{
		}

	public:
		constexpr PersistentArrayBT()
			: node(nullptr)
		{
		}

		constexpr PersistentArrayBT(Type * arr)
			: node(new Node(arr))
		{
		}

                PersistentArrayBT(const PersistentArrayBT & that)
                	: node(that.node)
                {
                	if(node != nullptr)
                		node->addRef();
                }

                PersistentArrayBT(PersistentArrayBT && that)
                	: node(that.node)
                {
                	that.node = nullptr;
                }

		~PersistentArrayBT()
		{
			if(node != nullptr)
			{
				Node::release(node);
				node = nullptr;
			}
		}

		const PersistentArrayBT & operator=(const PersistentArrayBT & that)
		{
			if(node != that.node)
			{
				if(node != nullptr)
					Node::release(node);

				node = that.node;
				node->addRef();
			}

			return *this;
		}

		const PersistentArrayBT & operator=(PersistentArrayBT && that)
		{
			if(node != that.node)
			{
				if(node != nullptr)
					Node::release(node);

				node = that.node;
				that.node = nullptr;
			}

			return *this;
		}

		const Type & operator[](size_t index) const
		{
			return Node::get(node, index);
		}

		const PersistentArrayBT set(size_t index, const Type & value) const
		{
			return PersistentArrayBT(Node::set(node, index, value));
		}
	};
} }
